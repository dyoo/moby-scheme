/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the License for the specific language governing rights and
 * limitations under the License.
 *
 * The Original Code is Bespin.
 *
 * The Initial Developer of the Original Code is Mozilla.
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Bespin Team (bespin@mozilla.com)
 *
 * ***** END LICENSE BLOCK ***** */

dojo.provide("bespin.editor.actions");

// = Actions =
//
// The editor can run various actions. They are defined here and you can add or change them dynamically. Cool huh?
//
// An action mutates the model or editor state in some way. The only way the editor state or model should be manipulated is via
// the execution of actions.
//
// Actions integrate with the undo manager by including instructions for how to undo (and redo) the action. These instructions
// take the form of a hash containing the necessary state for undo/redo. A key "action" corresponds to the function name of the
// action that should be executed to undo or redo the operation and the remaining keys correspond to state necessary to perform
// the action. See below for various examples.

dojo.declare("bespin.editor.Actions", null, {
    constructor: function(editor) {
        this.editor = editor;
        this.model = this.editor.model;
        this.cursorManager = this.editor.cursorManager;
        this.ignoreRepaints = false;
    },

    // this is a generic helper method used by various cursor-moving methods
    handleCursorSelection: function(args) {
        if (args.event.shiftKey) {
            if (!this.editor.selection) this.editor.setSelection({ startPos: bespin.editor.utils.copyPos(args.pos) });
            this.editor.setSelection({ startPos: this.editor.selection.startPos, endPos: bespin.editor.utils.copyPos(this.cursorManager.getCursorPosition()) });
        } else {
            this.editor.setSelection(undefined);
        }
    },

    moveCursor: function(moveType, args) {
        var posData = this.cursorManager[moveType](args);
        this.handleCursorSelection(args);
        this.repaint();
        args.pos = posData.newPos;
        return args;
    },

    moveCursorLeft: function(args) {
        return this.moveCursor("moveLeft", args);
    },

    moveCursorRight: function(args) {
        return this.moveCursor("moveRight", args);
    },

    moveCursorUp: function(args) {
        return this.moveCursor("moveUp", args);
    },

    moveCursorDown: function(args) {
        return this.moveCursor("moveDown", args);
    },

    moveToLineStart: function(args) {
        return this.moveCursor("moveToLineStart", args);
    },

    moveToLineEnd: function(args) {
        return this.moveCursor("moveToLineEnd", args);
    },

    moveToFileTop: function(args) {
        return this.moveCursor("moveToTop", args);
    },

    moveToFileBottom: function(args) {
        return this.moveCursor("moveToBottom", args);
    },

    movePageUp: function(args) {
        return this.moveCursor("movePageUp", args);
    },

    movePageDown: function(args) {
        return this.moveCursor("movePageDown", args);
    },

    moveWordLeft: function(args) {
        return this.moveCursor("smartMoveLeft", args);
    },

    moveWordRight: function(args) {
        return this.moveCursor("smartMoveRight", args);
    },

    deleteWordLeft: function(args) {
        this.deleteChunk({
            endPos: args.pos,
            pos: this.moveCursor("smartMoveLeft", args).pos
        });
        return args;
    },

    deleteWordRight: function(args) {
        this.deleteChunk({
            pos: args.pos,
            endPos: this.moveCursor("smartMoveRight", args).pos
        });
        return args;
    },

    undo: function() {
        this.editor.undoManager.undo();
    },

    redo: function() {
        this.editor.undoManager.redo();
    },

    selectAll: function(args) {
        // do nothing with an empty doc
        if (this.model.isEmpty()) return;

        args.startPos = { row: 0, col: 0 };
        args.endPos = { row: this.model.getRowCount() - 1, col: this.editor.ui.getRowScreenLength(this.model.getRowCount() - 1) };

        this.select(args);
    },

    select: function(args) {
        if (args.startPos) {
            this.editor.setSelection({ startPos: args.startPos, endPos: args.endPos });
            this.cursorManager.moveCursor(args.endPos);
        } else {
            this.editor.setSelection(undefined);
        }
    },

    insertTab: function(args) {
        var settings = bespin.get("settings");

        if (this.editor.getSelection() && !args.undoInsertTab) {
            this.indent(args);
            return;
        }

        var tab = args.tab;
        var tablength = this.cursorManager.getCharacterLength("\t");

        if (!tab || !tablength) {
            if (settings && settings.isSettingOn('tabmode')) {
                // do something tabby
                tab = "\t";
            } else {
                tab = "";
                var tabSizeCount = tablength;
                while (tabSizeCount-- > 0) {
                    tab += " ";
                }
                tablength = tab.length;
            }
        }

        delete this.editor.selection;
        this.model.insertCharacters(this.cursorManager.getModelPosition({ row: args.pos.row, col: args.pos.col }), tab);
        this.cursorManager.moveCursor({ row: args.pos.row, col: args.pos.col + tablength });
        this.repaint();

        // undo/redo
        args.action = "insertTab";
        var redoOperation = args;
        var undoArgs = {
            action: "removeTab",
            queued: args.queued,
            pos: bespin.editor.utils.copyPos(args.pos),
            tab: tab
        };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    // this function can only be called by editor.undoManager for undo insertTab in the case of beeing nothing selected
    removeTab: function(args) {
        delete this.editor.selection;
        this.model.deleteCharacters(this.cursorManager.getModelPosition({ row: args.pos.row, col: args.pos.col }), args.tab.length);
        this.cursorManager.moveCursor({ row: args.pos.row, col: args.pos.col });
        this.repaint();

        // undo/redo
        args.action = "removeTab";
        var redoOperation = args;
        var undoArgs = {
            action: "insertTab",
            undoInsertTab: true,
            queued: args.queued,
            pos: bespin.editor.utils.copyPos(args.pos),
            tab: args.tab
        };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    indent: function(args) {
        var historyIndent = args.historyIndent || false;
        var useHistoryIndent = !!historyIndent;
        if (!historyIndent) historyIndent = new Array();
        var settings = bespin.get('settings');
        var selection = args.selection || this.editor.getSelection();
        var fakeSelection = args.fakeSelection || false;
        var startRow = selection.startPos.row;
        var endRow = selection.endPos.row;
        var endRowLength = this.cursorManager.getStringLength(this.model.getRowArray(endRow).join(""));
        var cursorRowLength = this.cursorManager.getStringLength(this.model.getRowArray(args.pos.row).join(""));
        var charsToInsert;
        var tab = '';
        if (settings && settings.isSettingOn('tabmode')) {
            tab = "\t";
        } else {
            var tabsize = this.editor.getTabSize();
            while (tabsize-- > 0) {
                tab += " ";
            }
        }

        for (var y = startRow; y <= endRow; y++) {
            if (useHistoryIndent) {
                charsToInsert = historyIndent[y - startRow];
            } else if (tab != '\t') {
                charsToInsert = this.cursorManager.getLeadingWhitespace(y);
                charsToInsert = this.cursorManager.getNextTablevelRight(charsToInsert) - charsToInsert;
                charsToInsert = tab.substring(0, charsToInsert);
            } else {
                // in the case of "real" tabs we just insert the tabs
                charsToInsert = '\t';
            }
            this.model.insertCharacters(this.cursorManager.getModelPosition({ row: y, col: 0 }), charsToInsert);
            historyIndent[y - startRow] = charsToInsert;
        }

        var delta = this.cursorManager.getStringLength(this.model.getRowArray(args.pos.row).join("")) - cursorRowLength;
        if (!fakeSelection) {
            args.pos.col += delta;
            selection.endPos.col += this.cursorManager.getStringLength(this.model.getRowArray(endRow).join("")) - endRowLength;
            console.debug(selection.endPos);
            this.editor.setSelection(selection);
        } else {
            args.pos.col += delta;//(historyIndent[historyIndent.length-1] == '\t' ? this.editor.getTabSize() : historyIndent[historyIndent.length-1].length);
        }
        this.cursorManager.moveCursor({ col: args.pos.col });
        this.repaint();

        // undo/redo
        args.action = "indent";
        args.selection = selection;
        var redoOperation = args;
        var undoArgs = { action: "unindent", queued: args.queued, selection: selection, fakeSelection: fakeSelection, historyIndent: historyIndent, pos: bespin.editor.utils.copyPos(args.pos) };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    unindent: function(args) {
        var historyIndent = args.historyIndent || false;
        if (!historyIndent) {
            var newHistoryIndent = [];
        }
        var selection = args.selection || this.editor.getSelection();
        var fakeSelection = args.fakeSelection || false;
        if (!selection) {
            fakeSelection = true;
            selection = { startPos: { row: args.pos.row, col: args.pos.col }, endPos: { row: args.pos.row, col: args.pos.col } };
        }
        var startRow = selection.startPos.row;
        var endRow = selection.endPos.row;
        var endRowLength = this.cursorManager.getStringLength(this.model.getRowArray(endRow).join(""));
        var row = false;
        var charsToDelete;
        var charsWidth;

        for (var y = startRow; y <= endRow; y++) {
            if (historyIndent) {
                charsToDelete = historyIndent[y - startRow].length;
                charsWidth = (historyIndent[y - startRow] == '\t' ? this.editor.getTabSize() : historyIndent[y - startRow].length);
            } else {
                row = this.model.getRowArray(y);
                if (row.length > 0 && row[0] == '\t') {
                    charsToDelete = 1;
                    charsWidth = this.editor.getTabSize();
                } else {
                    var leadingWhitespaceLength = this.cursorManager.getLeadingWhitespace(y);
                    charsToDelete = this.cursorManager.getContinuousSpaceCount(0, this.editor.getTabSize(), y);
                    charsWidth = charsToDelete;
                }
                newHistoryIndent.push(row.join("").substring(0, charsToDelete));
            }

            if (charsToDelete) {
                this.model.deleteCharacters(this.cursorManager.getModelPosition({ row: y, col: 0 }), charsToDelete);
            }
            if (y == startRow) {
                selection.startPos.col = Math.max(0, selection.startPos.col - charsWidth);
            }
            if (y == endRow) {
                if (!row) row = this.model.getRowArray(y);
                var delta = endRowLength - this.cursorManager.getStringLength(row.join(""));
                selection.endPos.col = Math.max(0, selection.endPos.col - delta);
                args.pos.col = Math.max(0, args.pos.col - delta);
            }
        }
        this.cursorManager.moveCursor({ col: args.pos.col });

        if (!fakeSelection) {
            this.editor.setSelection(selection);
        }
        historyIndent = historyIndent ? historyIndent : newHistoryIndent;
        this.repaint();

        // undo/redo
        args.action = "unindent";
        args.selection = selection;
        var redoOperation = args;
        var undoArgs = { action: "indent", queued: args.queued, selection: selection, fakeSelection: fakeSelection, historyIndent: historyIndent, pos: bespin.editor.utils.copyPos(args.pos) };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    // NOTE: Actually, clipboard.js is taking care of this unless EditorOnly mode is set
    cutSelection: function(args) {
        this.copySelection(args);
        this.deleteSelection(args);
    },

    // NOTE: Actually, clipboard.js is taking care of this unless EditorOnly mode is set
    copySelection: function(args) {
        var selectionObject = this.editor.getSelection();
        if (selectionObject) {
            var selectionText = this.model.getChunk(selectionObject);
            if (selectionText) {
                bespin.editor.clipboard.Manual.copy(selectionText);
            }
        }
    },

    deleteSelectionAndInsertChunk: function(args) {
        var oldqueued = args.queued;

        args.queued = true;
        var selection = this.editor.getSelection();
        var chunk = this.deleteSelection(args);
        args.pos = bespin.editor.utils.copyPos(this.editor.getCursorPos());
        var endPos = this.insertChunk(args);

        args.queued = oldqueued;

        // undo/redo
        args.action = "deleteSelectionAndInsertChunk";
        args.selection = selection;
        var redoOperation = args;
        var undoArgs = {
            action: "deleteChunkAndInsertChunkAndSelect",
            pos: bespin.editor.utils.copyPos(args.pos),
            endPos: endPos,
            queued: args.queued,
            chunk: chunk
        };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    deleteChunkAndInsertChunkAndSelect: function(args) {
        var oldqueued = args.queued;

        args.queued = true;
        this.deleteChunk(args);
        this.insertChunkAndSelect(args);

        args.queued = oldqueued;

        // undo/redo
        args.action = "deleteChunkAndInsertChunkAndSelect";
        var redoOperation = args;
        var undoArgs = {
            action: "deleteSelectionAndInsertChunk",
            pos: bespin.editor.utils.copyPos(args.pos),
            queued: args.queued,
            selection: args.selection
        };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    // NOTE: Actually, clipboard.js is taking care of this unless EditorOnly mode is set
    pasteFromClipboard: function(args) {
        var clipboard = (args.clipboard) ? args.clipboard : bespin.editor.clipboard.Manual.data();
        if (clipboard === undefined) return; // darn it clipboard!
        args.chunk = clipboard;
        this.insertChunk(args);
    },

    insertChunk: function(args) {
        if (this.editor.selection) {
            this.deleteSelectionAndInsertChunk(args);
        } else {
            var pos = bespin.editor.utils.copyPos(this.cursorManager.getCursorPosition());
            pos = this.model.insertChunk(this.cursorManager.getModelPosition(pos), args.chunk);
            pos = this.cursorManager.getCursorPosition(pos);
            this.cursorManager.moveCursor(pos);
            this.repaint();

            // undo/redo
            args.action = "insertChunk";
            var redoOperation = args;
            var undoArgs = { action: "deleteChunk", pos: bespin.editor.utils.copyPos(args.pos), queued: args.queued, endPos: pos };
            var undoOperation = undoArgs;
            this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));

            return pos;
        }
    },

    deleteChunk: function(args) {
        var selection = this.editor.getSelection({ startPos: args.pos, endPos: args.endPos });
        var chunk = this.model.deleteChunk(selection);
        this.cursorManager.moveCursor(selection.startPos);
        this.repaint();

        // undo/redo
        args.action = "deleteChunk";
        var redoOperation = args;
        var undoArgs = { action: "insertChunk", pos: bespin.editor.utils.copyPos(selection.startPos), queued: args.queued, chunk: chunk };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    //deleteLine: function(args) {
    //    this.editor.lines.splice(args.pos.row);
    //    if (args.pos.row >= this.editor.lines.length) this.cursorManager.moveCursor({ row: args.pos.row - 1, col: args.pos.col });
    //    this.repaint();
    //},

    joinLine: function(args) {
        if (args.joinDirection == "up") {
            if (args.pos.row == 0) return;

            var newcol = this.editor.ui.getRowScreenLength(args.pos.row - 1);
            this.model.joinRow(args.pos.row - 1);
            this.cursorManager.moveCursor({ row: args.pos.row - 1, col: newcol });
        } else {
            if (args.pos.row >= this.model.getRowCount() - 1) return;

            this.model.joinRow(args.pos.row);
        }

        // undo/redo
        args.action = "joinLine";
        var redoOperation = args;
        var undoArgs = { action: "newline", pos: bespin.editor.utils.copyPos(this.editor.getCursorPos()), queued: args.queued };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));

        this.repaint();
    },

    killLine: function(args) {
        // select the current row
        this.editor.setSelection({ startPos: { row: args.pos.row, col: 0 }, endPos: { row: args.pos.row + 1, col: 0 } });
        this.cutSelection(args); // cut (will save and redo will work)
    },

    deleteSelection: function(args) {
        if (!this.editor.selection) return;
        var selection = this.editor.getSelection();
        var chunk = this.model.getChunk(selection);
        this.model.deleteChunk(selection);

        // undo/redo
        args.action = "deleteSelection";
        var redoOperation = args;
        var undoArgs = { action: "insertChunkAndSelect", pos: bespin.editor.utils.copyPos(selection.startPos), queued: args.queued, chunk: chunk };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));

        // setting the selection to undefined has to happen *after* we enqueue the undoOp otherwise replay breaks
        this.editor.setSelection(undefined);
        this.cursorManager.moveCursor(selection.startPos);
        this.repaint();

        return chunk;
    },

    insertChunkAndSelect: function(args) {
        var endPos = this.cursorManager.getCursorPosition(this.model.insertChunk(this.cursorManager.getModelPosition(args.pos), args.chunk));

        args.action = "insertChunkAndSelect";
        var redoOperation = args;
        var undoArgs = { action: "deleteSelection", pos: bespin.editor.utils.copyPos(endPos), queued: args.queued };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));

        // setting the selection to undefined has to happen *after* we enqueue the undoOp otherwise replay breaks
        this.editor.setSelection({ startPos: args.pos, endPos: endPos });
        this.cursorManager.moveCursor(endPos);
        this.repaint();
    },

    backspace: function(args) {
        if (this.editor.selection) {
            this.deleteSelection(args);
        } else {
            if (args.pos.col > 0) {
                var settings = bespin.get('settings');
                if (settings && settings.isSettingOn('smartmove')) {
                    var tabsize = this.editor.getTabSize();
                    var freeSpaces = this.cursorManager.getContinuousSpaceCount(args.pos.col, this.cursorManager.getNextTablevelLeft(args.pos.col));
                    if (freeSpaces == tabsize) {
                        var pos = args.pos;
                        this.editor.selection = { startPos: { row: pos.row, col: pos.col - tabsize}, endPos: {row: pos.row, col: pos.col}};
                        this.deleteSelection(args);
                        return;
                    }
                }
                this.cursorManager.moveCursor({ col:  Math.max(0, args.pos.col - 1) });
                args.pos.col -= 1;
                this.deleteCharacter(args);
            } else {
                args.joinDirection = "up";
                this.joinLine(args);
            }
        }
    },

    deleteKey: function(args) {
        if (this.editor.selection) {
            this.deleteSelection(args);
        } else {
            if (args.pos.col < this.editor.ui.getRowScreenLength(args.pos.row)) {
                var settings = bespin.get('settings');
                if (settings && settings.isSettingOn('smartmove')) {
                    var tabsize = this.editor.getTabSize();
                    var freeSpaces = this.cursorManager.getContinuousSpaceCount(args.pos.col, this.cursorManager.getNextTablevelRight(args.pos.col));
                    if (freeSpaces == tabsize) {
                        var pos = args.pos;
                        this.editor.selection = { startPos: { row: pos.row, col: pos.col}, endPos: {row: pos.row, col: pos.col + tabsize}};
                        this.deleteSelection(args);
                        return;
                    }
                }
                this.deleteCharacter(args);
            } else {
                args.joinDirection = "down";
                this.joinLine(args);
            }
        }
    },

    deleteCharacter: function(args) {
        if (args.pos.col < this.editor.ui.getRowScreenLength(args.pos.row)) {
            var modelPos = this.cursorManager.getModelPosition(args.pos);
            var deleted = this.model.deleteCharacters(modelPos, 1);
            this.repaint();

            // undo/redo
            args.action = "deleteCharacter";
            var redoOperation = args;
            var undoArgs = { action: "insertCharacter", pos: bespin.editor.utils.copyPos(args.pos), queued: args.queued, newchar: deleted };
            var undoOperation = undoArgs;
            this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
        }
    },

    newline: function(args) {
        var settings = bespin.get("settings");
        var autoindentAmount = (settings && settings.get('autoindent')) ? bespin.util.leadingSpaces(this.model.getRowArray(args.pos.row)) : 0;
        this.model.splitRow(this.cursorManager.getModelPosition(args.pos), autoindentAmount);
        this.cursorManager.moveCursor({ row: this.cursorManager.getCursorPosition().row + 1, col: autoindentAmount });

        // undo/redo
        args.action = "newline";
        var redoOperation = args;
        var undoArgs = { action: "joinLine", joinDirection: "up", pos: bespin.editor.utils.copyPos(this.cursorManager.getCursorPosition()), queued: args.queued };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));

        this.repaint();
    },

    // it seems kinda silly, but when you have a region selected and you insert a character, I have a separate action that is invoked.
    // this is because it's really two operations: deleting the selected region and then inserting a character. Each of these two
    // actions adds an operation to the undo queue. So I have two choices for
    deleteSelectionAndInsertCharacter: function(args) {
        var oldqueued = args.queued;

        args.queued = true;
        var chunk = this.deleteSelection(args);
        args.pos = bespin.editor.utils.copyPos(this.editor.getCursorPos());
        this.insertCharacter(args);

        args.queued = oldqueued;

        // undo/redo
        args.action = "deleteSelectionAndInsertCharacter";
        var redoOperation = args;
        var undoArgs = {
            action: "deleteCharacterAndInsertChunkAndSelect",
            pos: bespin.editor.utils.copyPos(args.pos),
            queued: args.queued,
            chunk: chunk
        };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    deleteCharacterAndInsertChunkAndSelect: function(args) {
        var oldqueued = args.queued;

        args.queued = true;
        this.deleteCharacter(args);
        this.insertChunkAndSelect(args);

        args.queued = oldqueued;

        // undo/redo
        args.action = "deleteCharacterAndInsertChunkAndSelect";
        var redoOperation = args;
        var undoArgs = { action: "deleteSelectionAndInsertCharacter", pos: bespin.editor.utils.copyPos(args.pos), queued: args.queued };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
    },

    insertCharacter: function(args) {
        if (this.editor.selection) {
            this.deleteSelectionAndInsertCharacter(args);
        } else {
            this.model.insertCharacters(this.cursorManager.getModelPosition(args.pos), args.newchar);
            this.cursorManager.moveRight(true);
            this.repaint();

            // undo/redo
            args.action = "insertCharacter";
            var redoOperation = args;
            var undoArgs = { action: "deleteCharacter", pos: bespin.editor.utils.copyPos(args.pos), queued: args.queued };
            var undoOperation = undoArgs;
            this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
        }
    },

    moveCursorRowToCenter: function(args) {
        var saveCursorRow = this.editor.getCursorPos().row;
        var halfRows = Math.floor(this.editor.ui.visibleRows / 2);
        if (saveCursorRow > (this.editor.ui.firstVisibleRow + halfRows)) { // bottom half, so move down
            this.cursorManager.moveCursor({ row: this.editor.getCursorPos().row + halfRows });
        } else { // top half, so move up
            this.cursorManager.moveCursor({ row: this.editor.getCursorPos().row - halfRows });
        }
        this.editor.ui.ensureCursorVisible();
        this.cursorManager.moveCursor({ row: saveCursorRow });
    },

    getOppositeCase: function(stringCase) {
        if (!stringCase) return undefined;

        switch (stringCase) {
            case 'u':
                return 'l';
            break;

            case 'l':
                return 'u';
            break;
        }
    },

    selectionChangeCase: function(args) {
        console.log('selectionChangeCase Fired!');
        if (this.editor.selection) {
            if (!args.selectionObject) {
                args.selectionObject = this.editor.getSelection();
            }

            var selection = this.model.getChunk(args.selectionObject);
            var stringArray = selection.split("\n");
            for (i in stringArray) {
                switch (args.stringCase) {
                    case 'l':
                        stringArray[i] = stringArray[i].toLowerCase();
                    break;

                    case 'u':
                        stringArray[i] = stringArray[i].toUpperCase();
                    break;
                }
            }
            var outText = stringArray.join("\n");

            this.model.deleteChunk(args.selectionObject);
            this.model.insertChunk(args.selectionObject.startModelPos, outText);
            this.select(args.selectionObject);

            args.action = "selectionChangeCase";
            var redoOperation = args;
            var undoArgs = { action: "undoSelectionChangeCase", selectionObject: args.selectionObject, text: selection, stringCase: this.getOppositeCase(args.stringCase) };
            var undoOperation = undoArgs;
            this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));
        }
    },

    undoSelectionChangeCase: function(args) {
        this.model.deleteChunk(args.selectionObject);
        var selection = this.model.insertChunk(args.selectionObject.startModelPos, args.text);
        this.select(args.selectionObject);

/*        args.action = "undoSelectionChangeCase";
        var redoOperation = args;
        var undoArgs = { action: "selectionChangeCase", selectionObject: args.selectionObject, text: selection, stringCase: this.getOppositeCase(args.stringCase) };
        var undoOperation = undoArgs;
        this.editor.undoManager.addUndoOperation(new bespin.editor.UndoItem(undoOperation, redoOperation));*/
    },

    // START SEARCH ACTIONS
    startSearch: function(str, displayType, shiftKey) {
        if (str == '') {
            // nothing to search for? Reset the searchString
            this.editor.ui.setSearchString(false);
            this.editor.paint(true);
            dojo.byId('searchresult').style.display = 'none';    
            return false;
        }
        
        if (str == this.editor.ui.searchString && displayType == 'toolbar') {
            if (!shiftKey) {
                this.findNext();    
            } else {
                this.findPrev();    
            }
            dojo.byId('searchresult').style.display = 'block';
            return;
        }
        
        // go and search for the searchString
        this.editor.ui.setSearchString(str);
        var count = this.editor.model.getCountOfString(str);
        if (count != 0) {
            // okay, there are matches, so go on...
            var pos = bespin.editor.utils.copyPos(this.editor.cursorManager.getCursorPosition());

            // first try to find the searchSting from the current position
            if (!this.editor.ui.actions.findNext(true)) {
                // there was nothing found? Search from the beginning
                this.editor.cursorManager.moveCursor({col: 0, row: 0 });
                this.editor.ui.actions.findNext();
            }
        }
        
        // display the count of matches in different ways
        switch(displayType) {
            case 'commandLine':
                var msg = "Found " + count + " match";
                if (count > 1) { msg += 'es'; }
                msg += " for your search for <em>" + str + "</em>";

                bespin.get('commandLine').showInfo(msg, true);
            break;
            
            case 'searchwindow':
                var filesearch = bespin.get('filesearch');
                if (filesearch) {
                    filesearch.setMatchesCount(count);
                }
            break;
            
            case 'toolbar':
                var msg = + count + " Match";
                if (count > 1) { msg += 'es'; }
                dojo.byId('searchfeedback').innerHTML = msg;
                dojo.byId('searchresult').style.display = 'block';
            break;
        }
        
        // repaint the editor
        this.editor.paint(true);
    },
    
    // find the next match in the file
    findNext: function(canBeSamePosition) {
        if (!this.editor.ui.searchString) return;
        var pos = bespin.editor.utils.copyPos(this.cursorManager.getModelPosition());
        var sel = this.editor.getSelection();
        if (canBeSamePosition && sel !== undefined) {
            pos.col -= sel.endModelPos.col - sel.startModelPos.col + 1;
        }
        var found = this.model.findNext(pos.row, pos.col, this.editor.ui.searchString);
        if (!found) found = this.model.findNext(0, 0, this.editor.ui.searchString);
        if (found) {
            this.editor.setSelection({startPos: this.cursorManager.getCursorPosition(found.startPos), endPos: this.cursorManager.getCursorPosition(found.endPos)});
            this.cursorManager.moveCursor(this.cursorManager.getCursorPosition(found.endPos));
            this.editor.ui.ensureCursorVisible();
            this.repaint();

            return true;
        } else {
            return false;
        }
    },

    // find the previous match in the file
    findPrev: function() {
        if (!this.editor.ui.searchString) return;

        var pos = this.cursorManager.getModelPosition();
        var found = this.model.findPrev(pos.row, pos.col, this.editor.ui.searchString);
        if (!found) {
            var lastRow = this.model.getRowCount() - 1;
            found = this.model.findPrev(lastRow, this.model.getRowArray(lastRow).length - 1, this.editor.ui.searchString);  
        } 
        if (found) {
            this.editor.setSelection({startPos: this.cursorManager.getCursorPosition(found.startPos), endPos: this.cursorManager.getCursorPosition(found.endPos)});
            this.cursorManager.moveCursor(this.cursorManager.getCursorPosition(found.endPos));
            this.editor.ui.ensureCursorVisible();
            this.repaint();
        }
    },

    // Fire an escape message so various parts of the UI can choose to clear
    escape: function() {
        bespin.publish("ui:escape");
    },
    // END SEARCH ACTIONS

    toggleQuickopen: function() {
        var quickopen = bespin.get('quickopen');
        if (quickopen) {
            quickopen.window.toggle();
        }
    },
    
    toggleFilesearch: function() {
        var settings = bespin.get("settings");
        
        if (settings && !settings.isSettingOn('searchwindow')) {
            dojo.byId('searchquery').focus();
            dojo.byId('searchquery').select();
        } else {
            var filesearch = bespin.get('filesearch');
            if (filesearch) {
                filesearch.window.toggle();
            }            
        }
    },
    
    focusCommandline: function() {
        var commandLine = bespin.get('commandLine');
        if (commandLine) {
            commandLine.commandLine.focus();
        }
    },

    repaint: function() {
        if (!this.ignoreRepaints) {
            this.editor.ui.ensureCursorVisible();
            this.editor.paint();
        }
    }
});