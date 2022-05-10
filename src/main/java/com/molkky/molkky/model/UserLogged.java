package com.molkky.molkky.model;

import com.molkky.molkky.domain.Tournament;
import lombok.*;
import type.UserRole;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UserLogged implements Serializable {

    private Integer id;
    private Integer tournamentRoleId;
    private String email;
    private String password;
    private UserRole role;
    private Tournament tournament;
    private TeamModel team;

    public UserLogged(Integer id, Integer tournamentRoleId, String email, String password, UserRole role, Tournament tournament) {
        this.id = id;
        this.email = email;
        this.password = password;
        this.role = role;
        this.tournament = tournament;
        this.tournamentRoleId = tournamentRoleId;
    }
}
