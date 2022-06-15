package com.molkky.molkky.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CreateTeamModel {

    private String name;
    private Integer nbPlayers;
    private Integer tournament;
    private Integer club;

    public CreateTeamModel(){
    }

    public CreateTeamModel(String name, Integer nbPlayers, Integer tournament, Integer club) {
        this.name = name;
        this.nbPlayers = nbPlayers;
        this.tournament = tournament;
        this.club = club;
    }
}
