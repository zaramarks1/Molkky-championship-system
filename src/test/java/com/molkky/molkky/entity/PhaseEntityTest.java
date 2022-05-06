package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Finnish;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.repository.*;
import org.checkerframework.checker.units.qual.A;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;
import type.TournamentStatus;
import type.UserRole;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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
    private UserTounamentRoleRepository userTounamentRoleRepository;

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithRounds() {
        Tournament tournament = new Tournament(
                "tournament test",
                "location",
                new Date(),
                new Date(),
                1,
                8,
                true,
                2,
                3
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
        phaseRepository.save(pool);

        List<Phase> phases = new ArrayList<>();
        phases.add(pool);
        tournament.setPhases(phases);
        tournamentRepository.save(tournament);


        for (int i =1; i <= 8; i++){
            Team team = new Team();

            team.setCode("12345");

            team.setName("Team" + i);
            team.setTournament(tournament);
            team = teamRepository.save(team);

            tournament.getTeams().add(team);

            tournamentRepository.save(tournament);

            User player = new User();

            player.setForename("User" + i);

           player =  userRepository.save(player);

            UserTounamentRole userTounamentRole = new UserTounamentRole();

            userTounamentRole.setRole(UserRole.PLAYER);
            userTounamentRole.setUser(player);
            userTounamentRole.setTournament(tournament);

            userTounamentRole = userTounamentRoleRepository.save(userTounamentRole);
        }

        Assertions.assertEquals(1, tournament.getPhases().size(), "Tournament should have 1 phase");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof Pool,
                " It should be a instance of pool");
        Assertions.assertEquals(8, tournament.getTeams().size(), " There should be 8 teams ");
    }
}
