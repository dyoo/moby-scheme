package com.phonegap.demo;

import android.test.ActivityInstrumentationTestCase;

/**
 * This is a simple framework for a test of an Application.  See
 * {@link android.test.ApplicationTestCase ApplicationTestCase} for more information on
 * how to write and extend Application tests.
 * <p/>
 * To run this test, you can type:
 * adb shell am instrument -w \
 * -e class com.phonegap.demo.DroidGapTest \
 * com.phonegap.demo.tests/android.test.InstrumentationTestRunner
 */
public class DroidGapTest extends ActivityInstrumentationTestCase<DroidGap> {

    public DroidGapTest() {
        super("com.phonegap.demo", DroidGap.class);
    }

}
