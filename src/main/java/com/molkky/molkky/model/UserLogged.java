package com.molkky.molkky.model;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import type.UserRole;

import java.io.Serializable;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class UserLogged implements Serializable {

    private Integer id;
    private String email;
    private String password;
    private UserRole role;
    private Team team;
    private Tournament tournament;

    public UserLogged(Integer id, String email, String password, UserRole role, Tournament tournament){
        this.id=id;
        this.email=email;
        this.password=password;
        this.role=role;
        this.tournament=tournament;
    }
}
