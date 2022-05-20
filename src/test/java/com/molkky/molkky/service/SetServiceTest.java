package com.molkky.molkky.service;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.model.SetModel;
import com.molkky.molkky.model.UserTournamentRoleModel;
import com.molkky.molkky.repository.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import type.TournamentStatus;
import type.UserRole;

import java.util.*;

import static org.mockito.Mockito.*;

@SpringBootTest
class SetServiceTest {
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private MatchService matchService;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;
    @Autowired
    private UserService userService;
    @Autowired
    private SetRepository setRepository;
    @Autowired
    private SetService setService;
    @MockBean
    private NotificationService notificationService;

    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private PhaseRepository phaseRepository;

    @Autowired
    RoundRepository roundRepository;

    @Autowired
    PhaseService phaseService;

    @Test
    void enterSetResultsTestTeam1() {
//        given
        Match match = createCompleteMatch2();
        Set set = match.getSets().get(0);
        UserTournamentRole user1 = match.getTeams().get(0).getUserTournamentRoles().get(0);
//        when
        set.setScore1Team1(25);
        set.setScore2Team1(15);
        setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user1));
//    then
        Assertions.assertEquals(25, set.getScore1Team1());
        Assertions.assertEquals(15, set.getScore2Team1());
    }

    @Test
    void enterSetResultsTestTeam2() {
//        given
        Match match = createCompleteMatch2();
        Set set = match.getSets().get(0);
        UserTournamentRole user2 = match.getTeams().get(1).getUserTournamentRoles().get(0);
//        when
        set.setScore1Team2(25);
        set.setScore2Team2(15);
        setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user2));
//    then
        Assertions.assertEquals(25, set.getScore1Team2());
        Assertions.assertEquals(15, set.getScore2Team2());
    }

    @Test
    void enterSetResultsTestTeamOrga() {
//        given
        Match match = createCompleteMatch2();
        Set set = match.getSets().get(0);
        UserTournamentRole user2 = userTournamentRoleRepository.save(new UserTournamentRole());
//        when
        set.setScore1Orga(25);
        set.setScore2Orga(15);
        setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user2));
//    then
        Assertions.assertEquals(25, set.getScore1Orga());
        Assertions.assertEquals(15, set.getScore2Orga());
    }

    @Test
    void getSetFromModelTest() {
//        given
        Set set = setRepository.save(new Set());
//        when
        Set setAfter = setService.getSetFromModel(SetService.createSetModel(set));
//    then
        Assertions.assertEquals(set.getId(), setAfter.getId());
    }

    @Test
    void createSetModelsTest() {
//        given
        List<Set> sets = new ArrayList<>();
        for(int i = 0; i < 3; i++) {
            sets.add(setRepository.save(new Set()));
        }
//        when
        List<SetModel> setModels = SetService.createSetModels(sets);
//    then
        Assertions.assertEquals(sets.size(), setModels.size());
        for (int i = 0; i < sets.size(); i++) {
            Assertions.assertEquals(sets.get(i).getId(), setModels.get(i).getId());
        }
    }

    @Test
    void createSetModelTest() {
//        given
        Set set = new Set();
        set.setScore1Team1(25);
        set.setScore2Team1(15);
        set.setScore1Team2(25);
        set.setScore2Team2(15);
        set.setScore1Orga(25);
        set.setScore2Orga(15);
//        when
        SetModel setModel = SetService.createSetModel(set);
//        then
        Assertions.assertEquals(25, setModel.getScore1Team1());
        Assertions.assertEquals(15, setModel.getScore2Team1());
        Assertions.assertEquals(25, setModel.getScore1Team2());
        Assertions.assertEquals(15, setModel.getScore2Team2());
        Assertions.assertEquals(25, setModel.getScore1Orga());
        Assertions.assertEquals(15, setModel.getScore2Orga());
    }

    @Test
    void isUserInSetTest() {
        Match match = createCompleteMatch2();
        User userIn = match.getTeams().get(0).getUserTournamentRoles().get(0).getUser();
        User userNotIn = userRepository.save(new User());
        Set set = match.getSets().get(0);
        Assertions.assertTrue(setService.isUserInSet(SetService.createSetModel(set), UserService.createUserModel(userIn)));
        Assertions.assertFalse(setService.isUserInSet(SetService.createSetModel(set), UserService.createUserModel(userNotIn)));
    }




    @Test
    void isMatchFinishedNotEqualNo50() {
        //        given
        Match match = createCompleteMatch2();
        Set set = match.getSets().get(0);
        UserTournamentRole user1 = match.getTeams().get(0).getUserTournamentRoles().get(0);
        UserTournamentRole user2 = match.getTeams().get(1).getUserTournamentRoles().get(0);
        User user3 = userRepository.save(new User());
        set = setRepository.findById(set.getId());

        //        when scores are equal but no team has 50
        set.setScore1Team1(25);
        set.setScore2Team1(15);
        set.setScore1Team2(25);
        set.setScore2Team2(15);
        setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user1));
        setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user2));
        //        then
        set = setRepository.findById(set.getId());
        Assertions.assertEquals(false, set.getFinished());
        verify(notificationService, times(0)).sendNotificationToList(anyString(), anyString(), anyList());
    }

    @Test
    void isMatchFinishedEqual50() {
        //        given
        Match match = createCompleteMatch2();
        Set set = match.getSets().get(0);
        UserTournamentRole user1 = match.getTeams().get(0).getUserTournamentRoles().get(0);
        UserTournamentRole user2 = match.getTeams().get(1).getUserTournamentRoles().get(0);
        User user3 = userRepository.save(new User());
        set = setRepository.findById(set.getId());

        //        when scores are equal and a team has 50
        set.setScore1Team1(50);
        set.setScore2Team1(20);
        set.setScore1Team2(50);
        set.setScore2Team2(20);
        setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user1));
        setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user2));
        //        then
        set = setRepository.findById(set.getId());
        Assertions.assertEquals(true, set.getFinished());

    }

    @Test
    void isMatchFinishedNotEqualNoStaffScores() {
        //        given
        Match match = createCompleteMatch2();
        Set set = match.getSets().get(0);
        UserTournamentRole user1 = match.getTeams().get(0).getUserTournamentRoles().get(0);
        UserTournamentRole user2 = match.getTeams().get(1).getUserTournamentRoles().get(0);
        User user3 = userRepository.save(new User());
        set = setRepository.findById(set.getId());


        //        when scores are not equal and staff has not entered scores
        set.setScore1Team1(50);
        set.setScore2Team1(20);
        set.setScore1Team2(30);
        set.setScore2Team2(40);
        setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user1));
        setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user2));
        //        then
        set = setRepository.findById(set.getId());
        Assertions.assertEquals(false, set.getFinished());
        verify(notificationService, times(1)).sendNotificationToList(anyString(), anyString(), anyList());
    }

    @Test
    void isMatchFinishedNotEqualStaffScores() {
        //        given
        Match match = createCompleteMatch2();
        Set set = match.getSets().get(0);
        UserTournamentRole user1 = match.getTeams().get(0).getUserTournamentRoles().get(0);
        UserTournamentRole user2 = match.getTeams().get(1).getUserTournamentRoles().get(0);
        UserTournamentRole user3 = userTournamentRoleRepository.save(new UserTournamentRole());
        set = setRepository.findById(set.getId());

        //        when scores are not equal and staff has entered scores
        set.setScore1Team1(50);
        set.setScore2Team1(20);
        set.setScore1Team2(30);
        set.setScore2Team2(40);
        set.setScore1Orga(50);
        set.setScore2Orga(20);
        setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user1));
        setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user2));
        setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user3));
        //        then
        set = setRepository.findById(set.getId());
        Assertions.assertEquals(true, set.getFinished());
    }

    Match createCompleteMatch() {

        Tournament tournament = createTournament();

        tournament = createSimpleGame(tournament, 1, false, false);

        Round round = new Round();
        round.setPhase(tournament.getPhases().get(0));

        //Match m = new Match();


        Match match = new Match();
        Team team1 = teamRepository.save(new Team());
        Team team2 = teamRepository.save(new Team());

        UserTournamentRole userTournamentRole1 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user1 = userRepository.save(new User());
        userTournamentRole1.setUser(user1);
        userTournamentRole1.setTeam(team1);
        userTournamentRoleRepository.save(userTournamentRole1);
        team1.setUserTournamentRoles(List.of(userTournamentRole1));
        team1 = teamRepository.save(team1);

        UserTournamentRole userTournamentRole2 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user2 = userRepository.save(new User());
        userTournamentRole2.setUser(user2);
        userTournamentRole2.setTeam(team2);
        userTournamentRoleRepository.save(userTournamentRole2);
        team2.setUserTournamentRoles(List.of(userTournamentRole2));
        team2 = teamRepository.save(team2);

        Set set1 = new Set();
        set1.setMatch(match);
        set1.setTeams(List.of(team1, team2));
        //setRepository.save(set1);

        match.setSets(List.of(set1));
        match.setTeams(Arrays.asList(team1, team2));
        // match = matchRepository.save(match);

        match.setRound(round);

        round.getMatches().add(match);

        round = roundRepository.save(round);
        return round.getMatches().get(0);
    }

    Match createCompleteMatch2(){
        Tournament tournament = createTournament();

        tournament = createPool(tournament);

        insertTeam(tournament, 2);
        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());

        return  tournament.getPhases().get(0).getRounds().get(0).getMatches().get(0);
    }


    Tournament createTournament(){
        Tournament tournament = new Tournament(
                "tournament test pool service",
                "location",
                new Date(),
                new Date(),
                1,
                8,
                true,
                2,
                3,
                2
        );
        tournament.setNbPlayersPerTeam(1);
        tournament.setVisible(true);
        tournament.setStatus(TournamentStatus.AVAILABLE);
        return tournamentRepository.save(tournament);

    }

    Tournament createSimpleGame(Tournament tournament, int nbPhase, boolean ranking, boolean seedingSystem){
        SimpleGame simpleGame = new SimpleGame();
        simpleGame.setNbSets(1);
        simpleGame.setTournament(tournament);
        simpleGame.setNbPhase(nbPhase);
        simpleGame.setRanking(ranking);
        simpleGame.setSeedingSystem(seedingSystem);

        simpleGame =  phaseRepository.save(simpleGame);

        tournament.getPhases().add(simpleGame);

        return tournamentRepository.save(tournament);
    }

    Tournament createPool(Tournament tournament){
        Pool pool = new Pool();

        pool.setNbSets(1);
        pool.setVictoryValue(2);
        pool.setNbPhase(1);
        pool.setNbPools(1);
        pool.setNbTeamsQualified(1);

        pool.setTournament(tournament);
        pool =  phaseRepository.save(pool);

        tournament.getPhases().add(pool);
        return tournamentRepository.save(tournament);
    }

    void insertTeam(Tournament tournament, int qtd) {
        for (int i = 1; i <= qtd; i++) {
            Team team = new Team();

            team.setCode("12345");

            team.setName("Team" + i);
            team.setTournament(tournament);


            tournament.getTeams().add(team);

            User player = new User();

            player.setForename("User" + i);

            player = userRepository.save(player);

            UserTournamentRole userTournamentRole = new UserTournamentRole();

            userTournamentRole.setRole(UserRole.PLAYER);
            userTournamentRole.setUser(player);
            userTournamentRole.setTournament(tournament);
            userTournamentRole.setTeam(team);

            player.getUserTournamentRoles().add(userTournamentRole);
            tournament.getUserTournamentRoles().add(userTournamentRole);
            team.getUserTournamentRoles().add(userTournamentRole);

            team = teamRepository.save(team);
            userTournamentRoleRepository.save(userTournamentRole);
            tournamentRepository.save(tournament);
            userRepository.save(player);
        }
    }


}
