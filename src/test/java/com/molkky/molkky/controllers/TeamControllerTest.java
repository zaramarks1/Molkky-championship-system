package com.molkky.molkky.controllers;

import com.molkky.molkky.model.AddPlayerModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.service.EmailSenderService;
import com.molkky.molkky.service.TeamService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.List;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringJUnit4ClassRunner.class)
public class TeamControllerTest {

    private MockMvc mockMvc;
    private AddPlayerModel test;

    @InjectMocks
    private TeamController teamController;

    @Mock
    private TeamRepository teamRepository;

    @Mock
    private TournamentRepository tournamentRepository;

    @Mock
    private UserRepository userRepository;

    @Mock
    private EmailSenderService emailSenderService;

    @Mock
    private TeamService teamService;

    @Mock
    private AddPlayerModel addPlayerModel;

    @Before
    public void setUp() throws Exception {
        mockMvc = MockMvcBuilders.standaloneSetup(teamController).build();
        Mockito.when(this.addPlayerModel.getTeamId()).thenReturn(1);
    }

    @Test
    public void testTeamController() throws Exception {

        Mockito.when(this.teamRepository.findById(addPlayerModel.getTeamId())).thenAnswer(i -> i.getArguments()[0]);

        mockMvc.perform(get("/team/create/")).andExpect(status().isOk());
        mockMvc.perform(post("/team/addPlayer/")).andExpect(status().isOk());
    }
}
