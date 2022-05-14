package com.molkky.molkky.service;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;

@WebMvcTest(value = TournamentService.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class TournamentServiceTest {

    String name = "test" + Math.random() * 10000;

    @Autowired
    private TournamentService tournamentService;

    @MockBean
    private TournamentModel tournamentModel;

    @MockBean
    private UserTournamentRole userTournamentRole;

    @MockBean
    private TournamentRepository tournamentRepository;

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @MockBean
    private Tournament tournament;

    @MockBean
    private User user;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        Mockito.when(tournamentModel.getName()).thenReturn(name);
    }

    @Test
    void testCreateCode() {
        String code = this.tournamentService.createCode(5);

        Assertions.assertEquals(5, code.length());
    }

    @Test
    void createTournamentServiceWithoutUser() {
        Mockito.when(this.tournamentModel.getName()).thenReturn("TEST 1");
        Mockito.when(this.tournamentRepository.save(Mockito.any(Tournament.class))).thenAnswer(i -> i.getArguments()[0]);

        Tournament test = this.tournamentService.create(this.tournamentModel);

        // Vérification de la création du tournoi et du nom qui est bien assigné
        Assertions.assertEquals("TEST 1", test.getName(), "Name incorrect");
    }

    @Test
    void createTournamentServiceWithUser() {
        Mockito.when(this.tournamentModel.getName()).thenReturn("TEST 1");
        Mockito.when(this.tournamentRepository.save(Mockito.any(Tournament.class))).thenAnswer(i -> i.getArguments()[0]);

        // On force le test vérifiant l'existence de l'utilisateur à true
        Mockito.when(this.userRepository.existsUserByEmail(this.user.getEmail())).thenReturn(true);

        Tournament test = this.tournamentService.create(this.tournamentModel);

        // Vérification de la création du tournoi et du nom qui est bien assigné
        Assertions.assertEquals("TEST 1", test.getName(), "Name incorrect");
    }
}

