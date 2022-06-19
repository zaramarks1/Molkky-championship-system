package com.molkky.molkky.model;


import com.molkky.molkky.domain.Team;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
@NoArgsConstructor
public class TeamFilterModel implements Serializable {
    private String name;

    public TeamFilterModel(Team team) {
        this.name = team.getName();

    }
}