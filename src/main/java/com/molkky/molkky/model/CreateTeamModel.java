package com.molkky.molkky.model;

import com.molkky.molkky.domain.User;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class CreateTeamModel {

    private String name;
    private Integer nbPlayers;
   // private List<User> users;

    public CreateTeamModel(){
    }

    public CreateTeamModel(String name, Integer nbPlayers) {
        this.name = name;
        this.nbPlayers = nbPlayers;

    }
}
