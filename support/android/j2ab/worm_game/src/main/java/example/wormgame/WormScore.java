/*
 *
 * Copyright (c) 2007, Sun Microsystems, Inc.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *  * Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *  * Neither the name of Sun Microsystems nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
/*
 * WormScore.java
 *
 * Created on March 30, 2001, 16:15
 * @version
 */
package example.wormgame;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import java.lang.InterruptedException;
import java.lang.Runnable;
import java.lang.System;

import javax.microedition.lcdui.Canvas;
import javax.microedition.lcdui.Font;
import javax.microedition.lcdui.Graphics;
import javax.microedition.rms.RecordStore;
import javax.microedition.rms.RecordStoreException;


/**
 * The WormScore keeps track of the high scores for each of the worm
 * levels. All access to the scores occur through static methods. There
 * can never be a WormScore object.
 *
 * We cache the scores and names for quick access during game play. The
 * calling application must first call openHighScores() to open the
 * score database. Finally, a closeHighScores() must be called to release
 * system resources.
 */
public class WormScore {
    /** Array of high scores for each game level. */
    private static short[] highScore = new short[WormPit.MAX_LEVELS];

    /** Array of player names for each high score level. */
    private static String[] highScoreName = new String[WormPit.MAX_LEVELS];

    /** Current score for this game. */
    private static RecordStore myStore;

    /** Is the internal score struct initialized? */
    private static boolean highScoresHaveBeenInit; /* = false; */

    /** Default constructor can not be instantiated. */
    private WormScore() {
    }

    /**
     * Initialize all high scores to 0.
     */
    private static void initializeScores() {
        /* Initialize the score store */
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        byte[] b;

        try {
            try {
                dos.writeShort(0);
                dos.writeUTF("");
                b = baos.toByteArray();
                dos.close();
            } catch (IOException ioe) {
                throw new RecordStoreException();
            }

            for (int i = 0; i < WormPit.MAX_LEVELS; i++) {
                myStore.addRecord(b, 0, b.length);
            }
        } catch (RecordStoreException rse) {
            /* Silently fail; exception to read high score is non-critical */
            closeHighScores();
        }
    }

    /**
     * Open the high score storage file. If the file doesn't exist,
     * initialize all high scores to 0.
     */
    static void openHighScores() {
        try {
            myStore = RecordStore.openRecordStore("HighScores", true);

            if (highScoresHaveBeenInit) {
                return;
            }

            /* Intialize the internal score structures */
            if (myStore.getNumRecords() == 0) {
                initializeScores();
            } else {
                /* Read high score store */
                ByteArrayInputStream bais;
                DataInputStream dis;
                byte[] data;

                for (int i = 0; i < WormPit.MAX_LEVELS; i++) {
                    data = myStore.getRecord(i + 1);

                    if (data != null) {
                        try {
                            bais = new ByteArrayInputStream(data);
                            dis = new DataInputStream(bais);
                            highScore[i] = dis.readShort();
                            highScoreName[i] = dis.readUTF();
                            dis.close();
                        } catch (IOException ioe) {
                        }
                    }
                }
            }

            highScoresHaveBeenInit = true;
        } catch (RecordStoreException rse) {
            /* Silently fail; exception to read high score is non-critical */
        }
    }

    /**
     * Close the high score file
     */
    static void closeHighScores() {
        if (myStore != null) {
            try {
                myStore.closeRecordStore();
            } catch (RecordStoreException frse) {
            }

            myStore = null;
        }
    }

    /**
     * Save high score for posterity.
     * @param level current game level
     * @param newScore current game score to be recorded
     * @param name current user name to be recorded
     */
    static void setHighScore(int level, int newScore, String name) {
        ByteArrayOutputStream baos;
        DataOutputStream das;
        byte[] data;

        /* Only save score if it's higher */
        if (newScore <= highScore[level]) {
            return;
        }

        try {
            try {
                baos = new ByteArrayOutputStream();
                das = new DataOutputStream(baos);

                das.writeShort((short)newScore);
                das.writeUTF(name);
                data = baos.toByteArray();
                das.close();
            } catch (IOException ioe) {
                throw new RecordStoreException();
            }

            if (myStore == null) {
                openHighScores();
                myStore.setRecord(level + 1, data, 0, data.length);
                closeHighScores();
            } else {
                myStore.setRecord(level + 1, data, 0, data.length);
            }
        } catch (RecordStoreException rse) {
            /* Silently fail; exception to save high score is non-critical */
        }

        highScore[level] = (short)newScore;
        highScoreName[level] = name;
    }

    /**
     * Return the high score for a given level.
     * @param level current level for high score check
     * @return numeric value for highest score at the
     * requested level
     */
    static short getHighScore(int level) {
        if (!highScoresHaveBeenInit) {
            openHighScores(); // Force scores to be initialized
            closeHighScores();
        }

        return highScore[level];
    }

    /**
     * Return the high score name for a given level.
     * @param level current level for high score check
     * @return name for highest score at the
     * requested level
     */
    static String getHighScoreName(int level) {
        if (!highScoresHaveBeenInit) {
            openHighScores(); // Force scores to be initialized
            closeHighScores();
        }

        return highScoreName[level];
    }
}
