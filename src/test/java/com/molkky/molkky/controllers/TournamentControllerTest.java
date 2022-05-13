package com.molkky.molkky.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.NotificationService;
import com.molkky.molkky.service.TournamentService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Arrays;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;


@WebMvcTest(value = TournamentController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class TournamentControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private TournamentRepository tournamentRepository;

    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @MockBean
    private NotificationService notificationService;

    @MockBean
    private TournamentService tournamentService;

    @MockBean
    private UserRepository userRepository;

    @Autowired
    private ObjectMapper objectMapper;

    @Mock
    private Tournament tournament;

    @Test
    void testTournamentController() throws Exception {
        mockMvc.perform(get("/tournament/create/"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("tournament"))
                .andExpect(view().name("/tournament/create"));

        when(tournamentService.create(any(TournamentModel.class))).thenReturn(this.tournament);
        when(this.tournament.getId()).thenReturn(5);

        mockMvc.perform(post("/tournament/create/")
                        .param("date", "2020-06-01")
                        .param("cutOffDate", "2020-03-01")
                        .flashAttr("tournament", new TournamentModel()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/tournament/5/view"));
        ;
        verify(tournamentService, times(1)).create(any(TournamentModel.class));
    }

    @Test
    void testTournamentControllerWithIdTournament() throws Exception {
        Tournament tournoi = new Tournament();
        tournoi.setTeams(Arrays.asList(new Team(), new Team()));

        when(this.tournamentRepository.findById(1)).thenReturn(tournoi);

        mockMvc.perform(get("/tournament/1/view")
                        .flashAttr("tournament", tournoi))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("tournament"))
                .andExpect(model().attributeExists("nbTeam"))

                .andExpect(view().name("/tournament/view"));

        verify(this.tournamentRepository, times(1)).findById(anyInt());
    }
}
