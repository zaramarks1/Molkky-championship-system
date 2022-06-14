package com.molkky.molkky.service;

import com.molkky.molkky.domain.Team;
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
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import type.TournamentStatus;
import type.UserRole;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import static com.molkky.molkky.utility.StringUtilities.createCode;

@WebMvcTest(value = TournamentService.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class TournamentServiceTest {

    String name = "test" + Math.random() * 10000;

    @Autowired
    private TournamentService tournamentService;

    @MockBean
    private TournamentModel tournamentModel;

    @MockBean
    private TournamentRepository tournamentRepository;

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @MockBean
    private User user;

    private String cutOffDate_string = "26-05-2022";
    private String date_string = "28-06-2023";

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        Mockito.when(tournamentModel.getName()).thenReturn(name);
    }

    @Test
    void testCreateCode() {
        String code = createCode(5);

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

    @Test
    void isMinimumTeamsBeforeDateTest() throws ParseException {
        List<Tournament> tournaments = new ArrayList<>();
        Tournament tournament1 = new Tournament();
        Tournament tournament2 = new Tournament();
        tournaments.add(tournament1);
        tournaments.add(tournament2);

        List<Team> teams = new ArrayList<>();
        Team team1 = new Team();
        Team team2 = new Team();
        teams.add(team1);
        teams.add(team2);

        tournament1.setTeams(teams);
        tournament1.setMinTeam(4);
        tournament2.setMinTeam(1);


        //Instantiating the SimpleDateFormat class
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
        //Parsing the given String to Date object
        Date cutOffDate = formatter.parse(cutOffDate_string);
        Date date = formatter.parse(date_string);

        tournament1.setCutOffDate(date);
        tournament1.setCutOffDate(cutOffDate);
        tournament2.setCutOffDate(date);
        tournament2.setCutOffDate(cutOffDate);

        Mockito.when(tournamentRepository.findAll()).thenReturn(tournaments);

        tournamentService.isMinimumTeamsBeforeDate();

        Assertions.assertEquals(TournamentStatus.CLOSED, tournament1.getStatus());
    }

    @Test
    void registerClosedForTournamentTest() throws ParseException {
        List<Tournament> tournaments = new ArrayList<>();
        Tournament tournament1 = new Tournament();
        tournaments.add(tournament1);

        //Instantiating the SimpleDateFormat class
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
        //Parsing the given String to Date object
        Date cutOffDate = formatter.parse(cutOffDate_string);
        Date date = formatter.parse(date_string);

        tournament1.setCutOffDate(date);
        tournament1.setCutOffDate(cutOffDate);
        tournament1.setStatus(TournamentStatus.AVAILABLE);
        tournament1.setName("tournoi");

        Mockito.when(tournamentRepository.findAll()).thenReturn(tournaments);
        Mockito.when(tournamentRepository.findByName("tournoi")).thenReturn(tournament1);

        tournamentService.registerClosedForTournament();

        Mockito.verify(tournamentRepository, Mockito.times(1)).save(tournament1);

        Assertions.assertFalse(tournamentRepository.findByName("tournoi").isRegisterAvailable());
    }

    @Test
    void getWinnersTest() throws Exception{
        Tournament tournament = new Tournament();
        Team team = new Team();
        team.setEliminated(false);
        List<Team> teams = new ArrayList<>();
        teams.add(team);
        tournament.setTeams(teams);

        List<Team> teamResult = tournamentService.getWinners(tournament);
        Assertions.assertEquals(teams, teamResult, "Winners are not correct");
    }

    @Test
    void testModifyTournament(){
        Tournament tournament = new Tournament();
        TournamentModel tournamentModel = new TournamentModel();
        tournamentModel.setId(1);
        tournamentModel.setName("Test");

        Mockito.when(tournamentRepository.findById(tournamentModel.getId())).thenReturn(tournament);
        Mockito.when(tournamentRepository.save(Mockito.any(Tournament.class))).thenAnswer(i -> i.getArguments()[0]);

        tournament = tournamentService.modifyTournament(tournamentModel);

        Assertions.assertNotNull(tournament);
        Assertions.assertEquals("Test",tournament.getName());

        Mockito.verify(tournamentRepository,Mockito.times(1)).findById(tournamentModel.getId());
        Mockito.verify(tournamentRepository,Mockito.times(1)).save(Mockito.any(Tournament.class));
    }

    @Test
    void testEmailAdmin(){
        Tournament tournament = new Tournament();
        tournament.setId(1);

        User user = new User();
        user.setId(1);
        user.setEmail("test@gmail.com");

        UserTournamentRole userTournamentRole = new UserTournamentRole();
        userTournamentRole.setUser(user);
        userTournamentRole.setTournament(tournament);

        Mockito.when(userTournamentRoleRepository.findUserTournamentRoleByRoleAndTournament(UserRole.ADM,tournament)).thenReturn(Arrays.asList(userTournamentRole));

        String mail = tournamentService.getEmailAdmin(tournament);

        Assertions.assertEquals("test@gmail.com",mail);

        Mockito.verify(userTournamentRoleRepository,Mockito.times(1)).findUserTournamentRoleByRoleAndTournament(UserRole.ADM,tournament);


    }
}

