package com.phonegap.demo;


public interface LifecycleService {
    void onPause();
    void onResume();
    void onStop();
    void onRestart();
    void onDestroy();
}