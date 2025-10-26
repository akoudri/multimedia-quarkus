package com.akfc.users.errors;

/**
 * Exception thrown when a requested user cannot be found.
 */
public class UserNotFoundException extends UserException {

    private final Long userId;
    private final String email;

    public UserNotFoundException(Long id) {
        super("User not found with ID: " + id);
        this.userId = id;
        this.email = null;
    }

    public UserNotFoundException(String email) {
        super("User not found with email: " + email);
        this.userId = null;
        this.email = email;
    }

    public UserNotFoundException(String message, Long userId) {
        super(message);
        this.userId = userId;
        this.email = null;
    }

    public Long getUserId() {
        return userId;
    }

    public String getEmail() {
        return email;
    }
}
