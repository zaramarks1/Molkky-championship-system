package com.molkky.molkky.model;

import com.molkky.molkky.domain.Tournament;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CreateTeamModel {

    private String name;
    private Integer nbPlayers;
    private Integer tournament;
   // private List<User> users;

    public CreateTeamModel(){
    }

    public CreateTeamModel(String name, Integer nbPlayers, Integer tournament) {
        this.name = name;
        this.nbPlayers = nbPlayers;
        this.tournament = tournament;
    }
}
