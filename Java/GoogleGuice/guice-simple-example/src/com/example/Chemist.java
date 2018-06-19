package com.example;

import com.example.model.IDrink;
import com.example.model.impl.Warehouse;
import com.google.inject.Guice;
import com.google.inject.Injector;

public class Chemist {
	public Chemist() {
		System.out.println("Chemist born");
		Injector injector = Guice.createInjector(new ProfitModule());

		// If you comment out the two lines below, Drug and Warehouse will not
		// get instantiated
		Warehouse warehouse = injector.getInstance(Warehouse.class);
		System.out.println(warehouse.checkInventory());

		IDrink drink = injector.getInstance(IDrink.class);
		drink.mix();
	}

	public static void main(String[] args) {
		new Chemist();
	}
}