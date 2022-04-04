package com.molkky.molkky.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class UserLogin {

    public String email;
    public String code;

    public UserLogin(){

    }

    public UserLogin(String email, String code) {
        this.email = email;
        this.code = code;
    }
}
