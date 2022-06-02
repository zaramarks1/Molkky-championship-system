package com.molkky.molkky.controllers;

import com.molkky.molkky.service.TournamentService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;

@WebMvcTest(value = ScheduleController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
public class ScheduleControllerTest {

    @MockBean
    private TournamentService tournamentService;

    @Test
    void testScheduleController() throws Exception{
        Mockito.verify(tournamentService, Mockito.times(1)).isMinimumTeamsBeforeDate();
        Mockito.verify(tournamentService, Mockito.times(1)).registerClosedForTournament();

        Mockito.verifyNoMoreInteractions(tournamentService);
    }
}
