package com.molkky.molkky.controllers;


import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleCustom;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import java.util.ArrayList;
import java.util.List;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = DisplayTeamController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class DisplayTeamControllerTest {

    @Autowired
    private MockMvc mockMvc;
    @MockBean
    private TeamRepository teamRepository;
    @MockBean
    private UserRepository userRepository;
    @MockBean
    private UserTournamentRoleCustom userTournamentRoleCustom;

    @Test
    void testGetTeams() throws Exception{
        mockMvc.perform(get("/team/displayTeams/"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("teamModel"))
                .andExpect(model().attributeExists("team"))
                .andExpect(model().attributeExists("teams"))
                .andExpect(view().name("team/displayTeams"));
        Team team = new Team();
        List<Team> teams= new ArrayList();
        teams.add(team);
        String name = "testEquipe";
        team.setName(name);
        Tournament tournament = new Tournament();
        tournament.setName("name");
        team.setTournament(tournament);
        //Mockito.when(Team.class.getName()).thenReturn(name);
        Mockito.when(teamRepository.existsTeamByName(Mockito.any())).thenReturn(true);
        Mockito.when(teamRepository.findTeamByName(Mockito.any())).thenReturn(teams);
        mockMvc.perform(post("/team/filter"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("teams"))
                .andExpect(view().name("/team/displayTeams"));
        Mockito.when(teamRepository.existsTeamByName(Mockito.any())).thenReturn(false);
        mockMvc.perform(post("/team/filter"))
                .andExpect(status().isOk())
                .andExpect(model().attributeDoesNotExist("teams"))
                .andExpect(view().name("/team/displayTeams"));
        Mockito.when(teamRepository.findById(Mockito.anyInt())).thenReturn(team);
        mockMvc.perform(get("/team/view").param("teamId", "1"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("team"))
                .andExpect(model().attributeExists("users"))
                .andExpect(view().name("/team/displayDetailsTeam"));

    }


}
