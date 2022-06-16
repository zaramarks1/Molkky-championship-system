package com.molkky.molkky.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CreateTeamModel {

    private String name;
    private Integer nbPlayers;
    private Integer tournament;
    private Integer clubId;
    private String newClubName;
    private String option;

    public CreateTeamModel(){
    }


}
