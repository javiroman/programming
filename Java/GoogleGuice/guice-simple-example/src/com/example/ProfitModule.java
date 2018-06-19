package com.example;

import com.example.model.ICreation;
import com.example.model.IDrink;
import com.example.model.IDrinkAdditive;
import com.example.model.impl.Coffee;
import com.example.model.impl.Drug;
import com.example.model.impl.Sugar;
import com.google.inject.AbstractModule;

public class ProfitModule extends AbstractModule {

	@Override
	protected void configure() {
		// No instantiation here
		bind(ICreation.class).to(Drug.class);
		bind(IDrinkAdditive.class).to(Sugar.class);
		// This instantiates Coffee right away, after this method is completed
		bind(IDrink.class).to(Coffee.class).asEagerSingleton();
		System.out.println("Profit module configured");
	}

}