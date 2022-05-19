package com.molkky.molkky.service.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.PhaseService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;
import type.PhaseType;
import type.TournamentStatus;
import type.UserRole;

import javax.transaction.Transactional;
import java.util.Date;
import java.util.List;
import java.util.Map;

@SpringBootTest(classes = MolkkyApplication.class)
 class SimpleGameServiceTest {

    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private PhaseRepository phaseRepository;

    @Autowired
    private TeamRepository teamRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    private PhaseService phaseService;

    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private MatchService matchService;

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithSimpleGameRound() {

        Tournament tournament = createTournament();

        tournament = createSimpleGame(tournament, 1, false, false);

        insertTeam(tournament, 8);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        Assertions.assertEquals(1, tournament.getPhases().size(), "Tournament should have 1 phase");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof SimpleGame,
                " It should be a instance of a simple game");

        Assertions.assertEquals(4, tournament.getPhases().get(0).getRounds().size(),
                " there should be 4 rounds in the phase");

        Assertions.assertEquals(8, tournament.getTeams().size(), " There should be 8 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");

        Assertions.assertEquals(4, results.size(), " There should be 4 rounds of simple game ");

        for(Map.Entry<Round, List<Match>> entry : results.entrySet()){

            Assertions.assertEquals(PhaseType.SIMPLEGAME, entry.getKey().getType(),
                    " The round should be of type simple game ");
            Assertions.assertEquals(2, entry.getKey().getTeams().size(), " The  should be 2 teams");
            Assertions.assertEquals(1, entry.getValue().size(), " The  should be one match");

        }

    }

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithSimpleGameRoundWith9Teams() {

        Tournament tournament = createTournament();

        tournament = createSimpleGame(tournament, 1, false, false);

        insertTeam(tournament, 9);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());

        Assertions.assertEquals(1, tournament.getPhases().size(), "Tournament should have 1 phase");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof SimpleGame,
                " It should be a instance of a simple game");

        Assertions.assertEquals(4, tournament.getPhases().get(0).getRounds().size(),
                " there should be 4 rounds in the phase");

        Assertions.assertEquals(9, tournament.getTeams().size(), " There should be 9 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");

        Assertions.assertEquals(4, results.size(), " There should be 4 rounds of simple game ");

        Assertions.assertEquals(1, tournament.getPhases().get(0).getRounds().get(0).getMatches().size(),
                " there should be 1 match in the first round");
        Assertions.assertEquals(3, tournament.getPhases().get(0).getRounds().get(3).getMatches().size(),
                " there should be 3 matches in the last");

        Assertions.assertEquals(2, tournament.getTeams().get(6).getMatchs().size(), " There should be 2 matches for team 7 ");
        Assertions.assertEquals(2, tournament.getTeams().get(7).getMatchs().size(), " There should be 2 matches for team 8 ");
        Assertions.assertEquals(2, tournament.getTeams().get(8).getMatchs().size(), " There should be 2 matches for team 9 ");

    }

    @Test
    @Rollback(false)
    @Transactional
    void validateMatchSimpleGame(){

        Tournament tournament = createTournament();

        tournament = createSimpleGame(tournament, 1, true, true);

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

    Tournament createTournament(){
        Tournament tournament = new Tournament(
                "tournament test simple game service",
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
        simpleGame.setNbSets(3);
        simpleGame.setTournament(tournament);
        simpleGame.setNbPhase(nbPhase);
        simpleGame.setRanking(ranking);
        simpleGame.setSeedingSystem(seedingSystem);

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
