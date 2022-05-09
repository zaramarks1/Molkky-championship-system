package com.molkky.molkky.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.service.TournamentService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


@WebMvcTest(value = TournamentController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
public class TournamentControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private TournamentRepository tournamentRepository;

    @MockBean
    private TournamentService tournamentService;

    @MockBean
    private UserRepository userRepository;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void testTournamentController() throws Exception {
        mockMvc.perform(get("/tournament/create/"))
                .andExpect(status().isOk());

        TournamentModel tournamentModel = new TournamentModel();
        tournamentModel.setDate("2019-01-01");        //tournamentModel.setCutOffDate("2022-06-01");
        tournamentModel.setCutOffDate("2020-01-01");

        mockMvc.perform(post("/tournament/create/")
                .param("tournament", String.valueOf(tournamentModel)))
                .andExpect(status().is3xxRedirection())
                .andExpect(model().attribute("tournament", tournamentModel));
        ;
        verify(tournamentService, times(1)).create(any(TournamentModel.class));
    }
}
