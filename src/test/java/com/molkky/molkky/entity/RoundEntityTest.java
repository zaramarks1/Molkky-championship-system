package com.molkky.molkky.entity;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.PhaseService;
import type.PhaseType;
import com.molkky.molkky.MolkkyApplication;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;
import type.TournamentStatus;
import type.UserRole;

import javax.transaction.Transactional;
import java.util.*;

@SpringBootTest(classes = MolkkyApplication.class)
class RoundEntityTest {
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

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithPoolRound() {

        Tournament tournament = createTournament();

        tournament = createPool(tournament, 1);

        insertTeam(tournament, 8);


        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());
        Assertions.assertEquals(1, tournament.getPhases().size(), "Tournament should have 1 phase");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof Pool,
                " It should be a instance of pool");
        Assertions.assertEquals(2, tournament.getPhases().get(0).getRounds().size(),
                " there should be 2 rounds in the phase");
        Assertions.assertEquals(8, tournament.getTeams().size(), " There should be 8 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");



        Assertions.assertEquals(1, tournament.getTeams().get(0).getRounds().size(),
                " There should be 1 round per team ");
        Assertions.assertEquals(2, results.size(), " There should be 2 rounds of pool ");


        Assertions.assertEquals(1, tournament.getTeams().get(0).getRounds().size(),
                " There should be 1 round per team ");
        Assertions.assertEquals(2, results.size(), " There should be 2 rounds of pool ");


    }

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithSimpleGameRound() {

        Tournament tournament = createTournament();

        tournament = createSimpleGame(tournament, 1);

        insertTeam(tournament, 8);


        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());
        Assertions.assertEquals(1, tournament.getPhases().size(), "Tournament should have 1 phase");
        //Assertions.assertEquals(1, tournament.getr, "Tournament should have 1 phase");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof SimpleGame,
                " It should be a instance of a simple game");
        //Assertions.assertEquals(4, tournament.getPhases().get(0).getRounds().size(),
        //        " there should be 4 rounds in the phase");
        Assertions.assertEquals(8, tournament.getTeams().size(), " There should be 8 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");
        //Assertions.assertEquals(4, results.size(), " There should be 4 rounds of simple game ");

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
