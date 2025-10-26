package com.akfc.catalog.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * DTO for User data received from the users service.
 * This is used when enriching catalog resources with user information.
 */
public class UserDto {

    public Long id;

    public String email;

    @JsonProperty("firstName")
    public String firstName;

    @JsonProperty("lastName")
    public String lastName;

    @JsonProperty("phoneNumber")
    public String phoneNumber;

    public String role;

    @JsonProperty("accountStatus")
    public String accountStatus;

    public UserDto() {
    }

    /**
     * Get the full name of the user.
     */
    public String getFullName() {
        return firstName + " " + lastName;
    }
}
