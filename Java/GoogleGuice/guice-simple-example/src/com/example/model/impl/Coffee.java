package com.example.model.impl;

import com.example.model.IDrink;
import com.example.model.IDrinkAdditive;
import com.google.inject.Inject;

public class Coffee implements IDrink {

	// An example of field injection
	@Inject
	private IDrinkAdditive drinkAdditive;

	public Coffee() {
		System.out.println("Coffee brewed");
	}

	@Override
	public String getName() {
		return "Folgers";
	}

	@Override
	public void mix() {
		System.out.println("Coffee mixed with " + this.drinkAdditive.getName());
	}
}
