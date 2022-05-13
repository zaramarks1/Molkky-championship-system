package com.molkky.molkky.model;

import lombok.Data;
import type.UserRole;

@Data
public class UserModel {
    private Integer id;
    private String pseudo;
    private String surname;
    private String forename;
    private String club;
    private String email;
    private Boolean registered;
    private UserRole role;
}
