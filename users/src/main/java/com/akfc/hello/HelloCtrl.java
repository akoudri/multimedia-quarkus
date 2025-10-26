package com.akfc.hello;

import jakarta.inject.Inject;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

@Path("/hello")
public class HelloCtrl {

    @Inject
    @Dev
    private HelloService hello;

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String hello() {
        return String.format("%s %s!", hello.message(), hello.name());
    }
}
