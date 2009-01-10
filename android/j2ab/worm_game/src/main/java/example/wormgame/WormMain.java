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
 * WormMain.java
 *
 * Created on March 30, 2001, 16:15
 * @version
 */
package example.wormgame;

import java.lang.Thread;

import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.CommandListener;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.Displayable;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.Gauge;
import javax.microedition.lcdui.Item;
import javax.microedition.midlet.MIDlet;
import javax.microedition.midlet.MIDletStateChangeException;


/**
 * Main routine for worm MIDlet.
 */
public class WormMain extends MIDlet implements CommandListener {
    /** Current game board for worm pit. */
    private WormPit theGame;

    /** Button for exiting the game. */
    private Command exitCmd = new Command("Exit", Command.EXIT, 3);

    /** Menu item for changing game levels. */
    private Command levelCmd = new Command("Change Level", Command.SCREEN, 2);

    /** Menu item for starting a new game. */
    private Command startCmd = new Command("Start", Command.SCREEN, 1);

    /** Menu item to restart another game. */
    private Command restartCmd = new Command("Restart", Command.SCREEN, 1);

    /** Enable audio. */
    private Command audioOnCmd = new Command("Audio On", Command.SCREEN, 1);

    /** Disable audio. */
    private Command audioOffCmd = new Command("Audio Off", Command.SCREEN, 1);

    /** Menu item to cancel current pausedmenu dialog. */
    private Command cancelCmd = new Command("Cancel", Command.ITEM, 1);

    /** Menu item to confirm current selected operation. */
    private Command OKCmd = new Command("OK", Command.OK, 1);
    private Thread myThread;

    /**
     * Default constructor for worm MIDlet game.
     * Creates the initial graphics objects and sets the command
     * listener.
     */
    public WormMain() {
        theGame = new WormPit();
        theGame.addCommand(exitCmd);
        theGame.addCommand(levelCmd);
        theGame.addCommand(startCmd);
        theGame.addCommand(audioOnCmd);
        theGame.setCommandListener(this);
    }

    /**
     * Destroy must cleanup everything. Only objects exist so the GC
     * will do all the cleanup after the last reference is removed.
     * @param unconditional if true, force MIDlet destroy processing
     */
    protected void destroyApp(boolean unconditional) {
        theGame.destroyAudioPlayer();
        theGame.destroyGame();
        Display.getDisplay(this).setCurrent((Displayable)null);
    }

    /**
     * Pause signals the thread to stop by clearing the thread field.
     * If stopped before done with the iterations it will be restarted
     * from scratch later.
     */
    protected void pauseApp() {
        theGame.mute();
        //stop game's timing thread
        theGame.destroyGame();

        try {
            //we will start another thread after this one finishes
            myThread.join();
        } catch (InterruptedException ie) {
            //
        }
    }

    /**
     * Start creates the thread to do the timing. It should return
     * immediately to keep the dispatcher from hanging.
     */
    protected void startApp() {
        theGame.unMute();
        Display.getDisplay(this).setCurrent(theGame);

        try {
            // Start the game in its own thread
            myThread = new Thread(theGame);
            //ensure the game thread will work after pause
            theGame.setDestroyed(false);
            myThread.start();
        } catch (Error e) {
            destroyApp(false);
            notifyDestroyed();
        }
    }

    /**
     * Respond to a commands issued on any Screen.
     * @param c command object source of action
     * @param d screen object containing the item the action was performed on
     */
    public void commandAction(Command c, Displayable d) {
        if (c == restartCmd) {
            theGame.restart();
        } else if (c == levelCmd) {
            Item[] levelItem = { new Gauge("Level", true, 9, theGame.getLevel()) };
            Form f = new Form("Change Level", levelItem);
            f.addCommand(OKCmd);
            f.addCommand(cancelCmd);
            f.setCommandListener(this);
            Display.getDisplay(this).setCurrent(f);
        } else if (c == exitCmd) {
            destroyApp(false);
            notifyDestroyed();
        } else if (c == startCmd) {
            theGame.removeCommand(startCmd);
            theGame.addCommand(restartCmd);
            theGame.restart();
        } else if (c == OKCmd) {
            Form f = (Form)d;
            Gauge g = (Gauge)f.get(0);
            theGame.setLevel(g.getValue());
            Display.getDisplay(this).setCurrent(theGame);
        } else if (c == cancelCmd) {
            Display.getDisplay(this).setCurrent(theGame);
        } else if (c == audioOnCmd) {
            /* Turn on Audio */
            theGame.createAudioPlayer();
            theGame.removeCommand(audioOnCmd);
            theGame.addCommand(audioOffCmd);
        } else if (c == audioOffCmd) {
            /* Turn off Audio */
            theGame.destroyAudioPlayer();
            theGame.removeCommand(audioOffCmd);
            theGame.addCommand(audioOnCmd);
        }
    }
}
