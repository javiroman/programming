package com.example.model.impl;

import com.example.model.ICreation;

public class Drug implements ICreation {

	public Drug() {
		System.out.println("Drug created");
	}

	public String identify() {
		return "Blue Drugs Identified";
	}
}