package com.molkky.molkky.controllers;


import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleCustom;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.NotificationService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.mock;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = DisplayTeamController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class DisplayTeamControllerTest {

    @Autowired
    private MockMvc mockMvc;
    @MockBean
    private TeamRepository teamRepository;
    @MockBean
    private NotificationService notificationService;
    @MockBean
    private UserRepository userRepository;
    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;


    @Test
    void testGetTeams() throws Exception{
        Team team = new Team();
        List<Team> teams= new ArrayList();
        teams.add(team);
        String name = "testEquipe";
        team.setName(name);
        //Mockito.when(Team.class.getName()).thenReturn(name);
        Mockito.when(teamRepository.findAll()).thenReturn(teams);
        Mockito.when(teamRepository.searchTeamsByName(Mockito.any(), Mockito.anyInt())).thenReturn(teams);
        mockMvc.perform(get("/team/displayTeams"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("teams"))
                .andExpect(view().name("team/displayTeams"));
        Mockito.verify(teamRepository, Mockito.times(1)).findAll();
        mockMvc.perform(get("/team/displayTeams?filter='bruh'"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("teams"))
                .andExpect(view().name("team/displayTeams"));
        Mockito.verify(teamRepository, Mockito.times(1)).searchTeamsByName(Mockito.any(), Mockito.anyInt());
    }

    @Test
    void testTeamView() throws Exception {
        String id = String.valueOf((int) Math.random() * 10000);
        Tournament tournament = mock(Tournament.class);
        Team team = new Team();
        team.setName("Test");
        team.setId(Integer.parseInt(id));
        team.setTournament(tournament);
        List<User> users = new ArrayList<>();
        User user = new User();
        users.add(user);

        Mockito.when(teamRepository.findById(Mockito.anyInt())).thenReturn(team);
        //Mockito.when(userTournamentRoleCustom.findUserByTeam(team)).thenReturn(users);

        mockMvc.perform(get("/team/view?teamId=" + id))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("team"))
                .andExpect(model().attributeExists("users"))
                .andExpect(view().name("/team/displayDetailsTeam"));
    }
}
