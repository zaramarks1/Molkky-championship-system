package com.molkky.molkky.service;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.model.CourtModel;
import com.molkky.molkky.model.MatchModel;
import com.molkky.molkky.model.UserTournamentRoleModel;
import com.molkky.molkky.repository.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import type.SetTeamIndex;
import type.TournamentStatus;

import java.util.*;

@SpringBootTest
class MatchServiceTest {
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private MatchService matchService;
    @Autowired
    private CourtRepository courtRepository;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private SetRepository setRepository;
    @Autowired
    private SetService setService;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private PhaseRepository phaseRepository;

    @Autowired
    private PhaseService phaseService;

    @Autowired
    private RoundRepository roundRepository;

    @Test
    void createMatchModelsTest() {
//        given
        List<Match> matches = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            matches.add(matchRepository.save(new Match()));
        }
//        when
        List<MatchModel> matchModels = MatchService.createMatchModels(matches);
//        then
        Assertions.assertEquals(10, matchModels.size());
        for (int i = 0; i < 10; i++) {
            Assertions.assertEquals(matches.get(i).getId(), matchModels.get(i).getId());
            Assertions.assertEquals(matches.get(i).getFinished(), matchModels.get(i).getFinished());
            Assertions.assertEquals(matches.get(i).getNbSets(), matchModels.get(i).getNbSets());
        }
    }

    @Test
    void createMatchModelTest() {
//        given
        Match match = matchRepository.save(new Match());
//        when
        MatchModel matchModel = MatchService.getMatchModelFromEntity(match);
//        then
        Assertions.assertEquals(match.getId(), matchModel.getId());
        Assertions.assertEquals(match.getFinished(), matchModel.getFinished());
        Assertions.assertEquals(match.getNbSets(), matchModel.getNbSets());
    }

    @Test
    void getMatchFromModelTest() {
//        given
        Match match = matchRepository.save(new Match());
        MatchModel matchModel = MatchService.getMatchModelFromEntity(match);
//        when
        Match match2 = matchService.getMatchFromModel(matchModel);
//        then
        Assertions.assertEquals(match.getId(), match2.getId());
        Assertions.assertEquals(match.getFinished(), match2.getFinished());
        Assertions.assertEquals(match.getNbSets(), match2.getNbSets());
    }

    @Test
    void getUserIndexTestAndIsInMatch1() {
//        given
        Match match = matchRepository.save(new Match());
        Team team1 = teamRepository.save(new Team());
        Team team2 = teamRepository.save(new Team());

        UserTournamentRole userTournamentRole1 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user1 = userRepository.save(new User());
        userTournamentRole1.setUser(user1);
        userTournamentRole1.setTeam(team1);
        userTournamentRoleRepository.save(userTournamentRole1);
        team1.setUserTournamentRoles(List.of(userTournamentRole1));
        team1 = teamRepository.save(team1);

        match.setTeams(Arrays.asList(team1, team2));
        match = matchRepository.save(match);
//        when
        SetTeamIndex index = matchService.getUserTeamIndex(MatchService.getMatchModelFromEntity(match), new UserTournamentRoleModel(userTournamentRole1));
//        then
        Assertions.assertEquals(SetTeamIndex.TEAM1, index);
        Assertions.assertTrue(matchService.isUserInMatch(MatchService.getMatchModelFromEntity(match), UserService.createUserModel(user1)));
    }

    @Test
    void getUserIndexTestAndIsInMatch2() {
//        given
        Match match = matchRepository.save(new Match());
        Team team1 = teamRepository.save(new Team());
        Team team2 = teamRepository.save(new Team());

        UserTournamentRole userTournamentRole1 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user1 = userRepository.save(new User());
        userTournamentRole1.setUser(user1);
        userTournamentRole1.setTeam(team1);
        userTournamentRoleRepository.save(userTournamentRole1);
        team1.setUserTournamentRoles(List.of(userTournamentRole1));
        team1 = teamRepository.save(team1);

        match.setTeams(Arrays.asList(team2, team1));
        match = matchRepository.save(match);
//        when
        SetTeamIndex index = matchService.getUserTeamIndex(MatchService.getMatchModelFromEntity(match), new UserTournamentRoleModel(userTournamentRole1));
//        then
        Assertions.assertEquals(SetTeamIndex.TEAM2, index);
        Assertions.assertTrue(matchService.isUserInMatch(MatchService.getMatchModelFromEntity(match), UserService.createUserModel(user1)));
    }

    @Test
    void getUserIndexTestAndIsInMatch3() {
//        given
        Match match = matchRepository.save(new Match());
        Team team1 = teamRepository.save(new Team());
        Team team2 = teamRepository.save(new Team());

        UserTournamentRole userTournamentRole1 = userTournamentRoleRepository.save(new UserTournamentRole());
        UserTournamentRole userTournamentRole2 = userTournamentRoleRepository.save(new UserTournamentRole());
        User user1 = userRepository.save(new User());
        User user2 = userRepository.save(new User());
        userTournamentRole2.setUser(user2);
        user2.setUserTournamentRoles(List.of(userTournamentRole2));
        userTournamentRole1.setUser(user1);
        userTournamentRole1.setTeam(team1);
        userTournamentRoleRepository.save(userTournamentRole1);
        userTournamentRoleRepository.save(userTournamentRole2);
        team1.setUserTournamentRoles(List.of(userTournamentRole1));
        team1 = teamRepository.save(team1);

        match.setTeams(Arrays.asList(team2, team1));
        match = matchRepository.save(match);
//        when
        SetTeamIndex index = matchService.getUserTeamIndex(MatchService.getMatchModelFromEntity(match), new UserTournamentRoleModel(userTournamentRole2));
//        then
        Assertions.assertEquals(SetTeamIndex.ORGA, index);
        Assertions.assertFalse(matchService.isUserInMatch(MatchService.getMatchModelFromEntity(match), UserService.createUserModel(user2)));
    }
    Match createCompleteMatch() {
        Tournament tournament = createTournament();

        tournament = createSimpleGame(tournament, 1, false, false);

        Round round = new Round();
        round.setPhase(tournament.getPhases().get(0));

        Match m = new Match();
        m.setRound(round);

        round.getMatches().add(m);

        round = roundRepository.save(round);

        Match match = round.getMatches().get(0);

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

        Set set1 = setRepository.save(new Set());
        set1.setMatch(match);
        set1.setTeams(List.of(team1, team2));
        setRepository.save(set1);

        match.setSets(List.of(set1));
        match.setTeams(Arrays.asList(team1, team2));
        match = matchRepository.save(match);
        return match;
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



    @Test
    void isMatchFinished(){
//        given
        Match match = createCompleteMatch();
        UserTournamentRole user1 = match.getTeams().get(0).getUserTournamentRoles().get(0);
        UserTournamentRole user2 = match.getTeams().get(1).getUserTournamentRoles().get(0);
        User user3 = userRepository.save(new User());
        //Set set = match.getSets().get(0);
//      when sets are finished
        for (Set set : match.getSets()){
            set = setRepository.findById(set.getId());
            set.setScore1Team1(50);
            set.setScore2Team1(20);
            set.setScore1Team2(50);
            set.setScore2Team2(20);
            setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user1));
            setService.enterSetResults(SetService.createSetModel(set), new UserTournamentRoleModel(user2));
            set = setRepository.findById(set.getId());
            Assertions.assertEquals(true, set.getFinished());
        }

//        then
        match = matchRepository.findById(match.getId());
        Assertions.assertEquals(true, match.getFinished());
        Assertions.assertEquals(50,match.getScoreTeam1(),"Le score de l'equipe 1 n'est pas a jour");
        Assertions.assertEquals(20,match.getScoreTeam2(),"Le score de l'equipe 2 n'est pas a jour");
    }

    @Test
    void setCourtTest(){
//        given
        Match match = matchRepository.save(new Match());
        Court court = courtRepository.save(new Court(true, "testCourt"));
        MatchModel matchModel = MatchService.getMatchModelFromEntity(match);
        CourtModel courtModel = new CourtModel(court);
//        when
        matchService.setCourt(matchModel, courtModel);
        match = matchRepository.findById(match.getId());
//        then
        Assertions.assertEquals(court.getId(), match.getCourt().getId());
    }
}
