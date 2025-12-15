package com.akfc.hello;

import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.container.ContainerRequestFilter;
import jakarta.ws.rs.ext.Provider;
import java.io.IOException;

@Provider
public class AuthFilter implements ContainerRequestFilter {

    @Override
    public void filter(ContainerRequestContext requestContext) throws IOException {
        String authHeader = requestContext.getHeaderString("Authorization");

        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            // Empêche l'accès à la ressource en cas d'absence de token
            // requestContext.abortWith(Response.status(Response.Status.UNAUTHORIZED).build());
            System.out.println("Requête non authentifiée.");
        } else {
            // Logique de validation du token...
            System.out.println("Requête avec token Bearer.");
        }
    }
}
