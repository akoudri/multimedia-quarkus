package com.akfc.hello;

import jakarta.enterprise.context.ApplicationScoped;

@ApplicationScoped
@Dev
@Logged
public class DevService implements HelloService {
    @Override
    public String message() {
        return "Hello";
    }

    @Override
    public String name() {
        return "Quarkus";
    }
}
