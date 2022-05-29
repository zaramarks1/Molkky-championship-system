package com.molkky.molkky.service;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.model.CourtModel;
import com.molkky.molkky.model.MatchModel;
import com.molkky.molkky.model.UserTournamentRoleModel;
import com.molkky.molkky.repository.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import type.PhaseType;
import type.SetTeamIndex;
import type.TournamentStatus;
import type.UserRole;

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

    
    @Test
    void isMatchFinished(){
//        given
        Match match = createCompleteMatch2();
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
