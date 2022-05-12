package com.molkky.molkky.service;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTounamentRole;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTounamentRoleRepository;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.runner.RunWith;
import org.mockito.*;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;
import org.mockito.stubbing.OngoingStubbing;

@RunWith(MockitoJUnitRunner.class)
public class TournamentServiceTest {

    String name = "test" + Math.random() * 10000;

    @InjectMocks
    private TounamentService tounamentService;

    @Mock
    private TournamentModel tournamentModel;

    @Mock
    private UserTounamentRole userTounamentRole;

    @Mock
    private TournamentRepository tournamentRepository;

    @Mock
    private UserRepository userRepository;

    @Mock
    private UserTounamentRoleRepository userTounamentRoleRepository;

    @Mock
    private Tournament tournament;

    @Mock
    private User user;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        Mockito.when(tournamentModel.getName()).thenReturn(name);
    }

    @Test
    public void testCreateCode() {
        String code = this.tounamentService.createCode(5);

        Assertions.assertEquals(5, code.length());
    }

    @Test
    public void createTournamentServiceWithoutUser() {
        Mockito.when(this.tournamentModel.getName()).thenReturn("TEST 1");
        Mockito.when(this.tournamentRepository.save(Mockito.any(Tournament.class))).thenAnswer(i -> i.getArguments()[0]);

        Tournament test = this.tounamentService.create(this.tournamentModel);

        // Vérification de la création du tournoi et du nom qui est bien assigné
        Assertions.assertEquals("TEST 1", test.getName(), "Name incorrect");
    }

    @Test
    public void createTournamentServiceWithUser() {
        Mockito.when(this.tournamentModel.getName()).thenReturn("TEST 1");
        Mockito.when(this.tournamentRepository.save(Mockito.any(Tournament.class))).thenAnswer(i -> i.getArguments()[0]);

        // On force le test vérifiant l'existence de l'utilisateur à true
        Mockito.when(this.userRepository.existsUserByEmail(this.user.getEmail())).thenReturn(true);

        Tournament test = this.tounamentService.create(this.tournamentModel);

        // Vérification de la création du tournoi et du nom qui est bien assigné
        Assertions.assertEquals("TEST 1", test.getName(), "Name incorrect");
    }
}
