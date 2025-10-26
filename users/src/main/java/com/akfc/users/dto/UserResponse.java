package com.akfc.users.dto;

import com.akfc.users.data.AccountStatus;
import com.akfc.users.data.User;
import com.akfc.users.data.UserRole;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * DTO for user response.
 * Contains user information for API responses (excludes sensitive data).
 */
public class UserResponse {

    public Long id;
    public String email;
    public String firstName;
    public String lastName;
    public String phoneNumber;
    public LocalDate registrationDate;
    public UserRole role;
    public AccountStatus accountStatus;
    public LocalDateTime lastLoginAt;
    public Boolean archived;
    public LocalDateTime createdAt;
    public LocalDateTime updatedAt;

    /**
     * Create response DTO from entity.
     */
    public static UserResponse from(User user) {
        UserResponse response = new UserResponse();
        response.id = user.id;
        response.email = user.email;
        response.firstName = user.firstName;
        response.lastName = user.lastName;
        response.phoneNumber = user.phoneNumber;
        response.registrationDate = user.registrationDate;
        response.role = user.role;
        response.accountStatus = user.accountStatus;
        response.lastLoginAt = user.lastLoginAt;
        response.archived = user.archived;
        response.createdAt = user.createdAt;
        response.updatedAt = user.updatedAt;
        return response;
    }

    /**
     * Get full name.
     */
    public String getFullName() {
        return firstName + " " + lastName;
    }
}
