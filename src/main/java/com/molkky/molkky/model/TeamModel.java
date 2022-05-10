package com.molkky.molkky.model;

import com.molkky.molkky.domain.*;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class TeamModel implements Serializable {
    private Integer id;
    private String name;
    private Integer nbPlayers;
    private Integer nbWins = 0;

    public TeamModel(Team team) {
        this.id = team.getId();
        this.name = team.getName();
        this.nbPlayers = team.getNbPlayers();
        this.nbWins = team.getNbWins();
    }

    public static List<TeamModel> createTeamModels(List<Team> teams) {
        List<TeamModel> teamModels = new ArrayList<>();
        for (Team team : teams) {
            if(team.getId() != null) {
                teamModels.add(new TeamModel(team));
            }
        }
        return teamModels;
    }
}
