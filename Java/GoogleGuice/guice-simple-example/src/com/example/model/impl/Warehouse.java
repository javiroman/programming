package com.example.model.impl;

import com.example.model.ICreation;
import com.google.inject.Inject;

public class Warehouse {

	private final ICreation creation;

	// Constructor injection example
	@Inject
	public Warehouse(ICreation creation) {
		// The creation is instantiated here, since it is a constructor
		// injection parameter
		this.creation = creation;
		System.out.println("Warehouse assembled");
	}

	public String checkInventory() {
		return creation.identify();
	}

	public ICreation getCreation() {
		return creation;
	}
}