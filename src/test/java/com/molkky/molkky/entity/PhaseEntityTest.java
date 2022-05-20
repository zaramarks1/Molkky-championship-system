package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.repository.*;
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
import java.util.*;

@SpringBootTest(classes = MolkkyApplication.class)
 class PhaseEntityTest {

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
    void testInsertTournamentWithRound() {
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
         tournament= tournamentRepository.save(tournament);

        Pool pool = new Pool();

        pool.setNbSets(1);
        pool.setVictoryValue(2);
        pool.setNbPhase(1);
        pool.setNbPools(2);
        pool.setNbTeamsQualified(4);

        pool.setTournament(tournament);
        pool =  phaseRepository.save(pool);

        SimpleGame simpleGame = new SimpleGame();

        simpleGame.setNbPhase(2);
        simpleGame.setTournament(tournament);
        simpleGame.setNbSets(3);

        simpleGame = phaseRepository.save(simpleGame);

        List<Phase> phases = new ArrayList<>();
        phases.add(pool);
        phases.add(simpleGame);
        tournament.setPhases(phases);
        tournamentRepository.save(tournament);



        for (int i =1; i <= 8; i++){
            Team team = new Team();

            team.setCode("12345");

            team.setName("Team" + i);
            team.setTournament(tournament);


            tournament.getTeams().add(team);

            User player = new User();

            player.setForename("User" + i);

            player =  userRepository.save(player);

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

        HashMap<Round, List<Match>> results = (HashMap<Round, List<Match>>) phaseService.generate(pool.getId().toString());
        Assertions.assertEquals(2, tournament.getPhases().size(), "Tournament should have 1 phase");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof Pool,
                " It should be a instance of pool");
        Assertions.assertEquals(8, tournament.getTeams().size(), " There should be 8 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");
        Assertions.assertEquals(2, results.size(), " There should be 2 rounds of pool ");

        for(Map.Entry<Round, List<Match>> entry : results.entrySet()){

            Assertions.assertEquals(PhaseType.POOL, entry.getKey().getType(),
                    " The round should be of type pool ");
            Assertions.assertEquals(4, entry.getKey().getTeams().size(), " The  should be 4 teams");
            Assertions.assertEquals(6, entry.getValue().size(), " The  should be 6 matches");

        }
        Assertions.assertEquals(2, tournament.getPhases().size(), "Tournament should have 2 phases");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof Pool,
                " It should be a instance of pool");
        Assertions.assertEquals(true, tournament.getPhases().get(1) instanceof SimpleGame,
                " It should be a instance of simple game");
    }
}
