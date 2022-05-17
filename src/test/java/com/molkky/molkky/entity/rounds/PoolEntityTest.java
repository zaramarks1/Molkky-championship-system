package com.molkky.molkky.entity.rounds;

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
class PoolEntityTest {
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
    void testPoolRound() {
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


        List<Phase> phases = new ArrayList<>();
        phases.add(pool);
        tournament.setPhases(phases);
        tournamentRepository.save(tournament);


        Assertions.assertEquals(1, tournament.getPhases().size(), "Tournament should have 1 phase");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof Pool,
                " It should be a instance of pool");

    }
}
