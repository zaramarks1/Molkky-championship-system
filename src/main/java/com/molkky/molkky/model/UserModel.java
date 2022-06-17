package com.molkky.molkky.model;

import com.molkky.molkky.domain.Club;
import lombok.Data;
import type.UserRole;

@Data
public class UserModel {
    private Integer id;
    private String pseudo;
    private String surname;
    private String forename;
    private Club club;
    private String email;
    private Boolean registered;
    private UserRole role;
}
