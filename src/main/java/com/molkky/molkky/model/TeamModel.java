package com.molkky.molkky.model;

import com.molkky.molkky.domain.*;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class TeamModel {
    private Integer id;
    private String name;
    private Integer nbPlayers;
    private List<Set> sets;
    private List<Match> matchs;
    private List<Round> rounds;
    private List<User> users;
    private Tournament tournament;
    private List<Shot> shots;
    private Integer nbWins = 0;

    public TeamModel(Team team) {
        this.id = team.getId();
        this.name = team.getName();
        this.nbPlayers = team.getNbPlayers();
        this.sets = team.getSets();
        this.matchs = team.getMatchs();
        this.rounds = team.getRounds();
        this.users = team.getUsers();
        this.tournament = team.getTournament();
        this.shots = team.getShots();
        this.nbWins = team.getNbWins();
    }

    public static List<TeamModel> createTeamModels(List<Team> teams) {
        List<TeamModel> teamModels = new ArrayList<>();
        for (Team team : teams) {
            teamModels.add(new TeamModel(team));
        }
        return teamModels;
    }
}
