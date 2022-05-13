package com.molkky.molkky.models;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.model.TeamModel;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

class TeamModelTest {

    @Test
    void testDefaultConstructor() {
        TeamModel teamModel = new TeamModel();
        Assertions.assertNotNull(teamModel);
    }

    @Test
    void testTeamConstructor() {
        Team team = new Team();
        team.setName("Team 1");
        team.setId(1);
        team.setNbPlayers(2);
        team.setNbWins(1);
        TeamModel teamModel = new TeamModel(team);
        Assertions.assertEquals("Team 1", teamModel.getName());
        Assertions.assertEquals(1, teamModel.getId());
        Assertions.assertEquals(2, teamModel.getNbPlayers());
        Assertions.assertEquals(1, teamModel.getNbWins());
    }

    @Test
    void testTeamConstructorMultiple() {
        List<Team> teams = new ArrayList<>();
        Team team1 = new Team();
        team1.setName("Team 1");
        team1.setId(1);
        team1.setNbPlayers(2);
        team1.setNbWins(1);
        teams.add(team1);
        Team team2 = new Team();
        team2.setName("Team 2");
        team2.setId(2);
        team2.setNbPlayers(2);
        team2.setNbWins(1);
        teams.add(team2);
        List<TeamModel> teamModels = TeamModel.createTeamModels(teams);
        Assertions.assertEquals(2, teamModels.size());
    }
}
