package com.molkky.molkky.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CreateTeamModel {

    private String name;
    private Integer nbPlayers;
    private Integer tournament;

    public CreateTeamModel(){
    }

    public CreateTeamModel(String name, Integer nbPlayers, Integer tournament) {
        this.name = name;
        this.nbPlayers = nbPlayers;
        this.tournament = tournament;
    }
}
