package com.phonegap.demo;

// A small structure for saving cell strength info.

public class CellInfo {
    private int id;
    private int strength;

    public CellInfo(int id, int strength) {
	this.id = id;
	this.strength = strength;
    }

    public int getId() { return this.id; }
    public int getStrength() { return this.strength; }
}