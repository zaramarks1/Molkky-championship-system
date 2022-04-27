package com.molkky.molkky.model;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class UserLogged {

    private String email;
    private String password;
    private String role;
    private Team team;
    private Tournament tournament;
}
