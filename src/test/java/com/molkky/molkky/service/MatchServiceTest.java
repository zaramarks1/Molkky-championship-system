package com.molkky.molkky.service;

import com.google.j2objc.annotations.AutoreleasePool;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.model.MatchModel;
import com.molkky.molkky.model.UserTournamentRoleModel;
import com.molkky.molkky.repository.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;
import type.SetTeamIndex;
import type.TournamentStatus;
import type.UserRole;

import javax.transaction.Transactional;
import java.util.*;

@SpringBootTest
class MatchServiceTest {
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
    @Rollback(false)
    @Transactional
    void validateMatchSimpleGame(){

        Tournament tournament = createTournament();

        tournament = createSimpleGame(tournament, 1);

        insertTeam(tournament, 8);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());

        for (Round r: tournament.getPhases().get(0).getRounds()){
            r.getMatches().get(0).setFinished(true);
            r.getMatches().get(0).setWinner(r.getMatches().get(0).getTeams().get(0));
            matchRepository.save(r.getMatches().get(0));
            matchService.validateMatch(r.getMatches().get(0));
        }

        List<Team> teams = teamRepository.findByTournamentAndEliminated(tournament,false);

        Assertions.assertEquals(8, tournament.getTeams().size(), "there should be 8 teams");
        Assertions.assertEquals(4, teams.size(), "there should be 4 teams remaining");

    }

    @Test
    @Rollback(false)
    @Transactional
    void validateMatchPool(){

        Tournament tournament = createTournament();

        tournament = createPool(tournament, 1);

        insertTeam(tournament, 6);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());

        List<Round> rounds = new ArrayList<>(tournament.getPhases().get(0).getRounds());
        for (Round r: rounds){
           for(Match m : r.getMatches()){
               Random rand = new Random();
               m.setFinished(true);
               m.setWinner(m.getTeams().get(0));
               m.setScoreTeam1(50);
               m.setScoreTeam2(rand.nextInt(49));
               matchRepository.save(m);
               matchService.validateMatch(m);
           }


        }
        List<Team> teams = teamRepository.findByTournamentAndEliminated(tournament,false);

        Assertions.assertEquals(6, tournament.getTeams().size(), "there should be 6 teams");
        Assertions.assertEquals(4, teams.size(), "there should be 4 teams remaining");
        Assertions.assertTrue( tournament.getTeams().get(4).isEliminated(), "TEAM 5 should be eliminated");
        Assertions.assertTrue( tournament.getTeams().get(5).isEliminated(), "TEAM 6 should be eliminated");
        Assertions.assertEquals(4, teams.size(), "there should be 4 teams remaining");
        Assertions.assertEquals(1, teams.get(0).getUserTournamentRoles().get(0).getNotifications().size(), "each player must have one notification");

    }

    Tournament createTournament(){
        Tournament tournament = new Tournament(
                "tournament test",
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
    Tournament createPool(Tournament tournament, int nbPhase){
        Pool pool = new Pool();

        pool.setNbSets(1);
        pool.setVictoryValue(2);
        pool.setNbPhase(nbPhase);
        pool.setNbPools(2);
        pool.setNbTeamsQualified(4);
        pool.setSeedingSystem(true);

        pool.setTournament(tournament);
        pool =  phaseRepository.save(pool);

        tournament.getPhases().add(pool);
        return tournamentRepository.save(tournament);
    }

    Tournament createSimpleGame(Tournament tournament, int nbPhase){
        SimpleGame simpleGame = new SimpleGame();
        simpleGame.setNbSets(3);
        simpleGame.setTournament(tournament);
        simpleGame.setNbPhase(nbPhase);

        simpleGame =  phaseRepository.save(simpleGame);

        tournament.getPhases().add(simpleGame);

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
