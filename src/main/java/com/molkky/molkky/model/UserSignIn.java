package com.molkky.molkky.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class UserSignIn {
    public String email;
    public String code;

    public UserSignIn(){

    }

    public UserSignIn(String email) {
        this.email = email;
    }
}
