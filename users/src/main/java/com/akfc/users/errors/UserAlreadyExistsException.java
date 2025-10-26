package com.akfc.users.errors;

/**
 * Exception thrown when attempting to create a user with an email that already exists.
 */
public class UserAlreadyExistsException extends UserException {

    private final String email;

    public UserAlreadyExistsException(String email) {
        super("User already exists with email: " + email);
        this.email = email;
    }

    public String getEmail() {
        return email;
    }
}
