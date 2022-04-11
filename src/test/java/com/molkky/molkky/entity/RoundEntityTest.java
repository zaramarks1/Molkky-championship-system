package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.repository.RoundRepository;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;

import javax.transaction.Transactional;
import java.util.*;

@SpringBootTest(classes = MolkkyApplication.class)
class RoundEntityTest {
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private TournamentRepository tournamentRepository;

    @Test
    @Transactional
    @Rollback(false)
    void testInsertUser() {
        Tournament tournament = tournamentRepository.save(new Tournament(
                "tournament_name",
                "location",
                new Date(),
                new Date(),
                1,
                2,
                true,
                2,
                3
        ));

        Round round = new Round("pool", 2);
        round.setTournament(tournament);

        Team team1 = teamRepository.save(new Team("team1_test", 1));
        Team team2 = teamRepository.save(new Team("team2_test", 2));

        List<Team> teams = new ArrayList<>();
        teams.add(team1);
        teams.add(team2);

        round.setTeams(teams);

        Round newRound = roundRepository.save(round);

        Assertions.assertEquals("pool", round.getType(), "Type is not correct");
        Assertions.assertEquals(2, round.getTeams().size(), "Team size is not correct");
        Round recupRound = roundRepository.findById(round.getId());
        Assertions.assertEquals(recupRound.getType(), round.getType(), "Type is not correct");
        Assertions.assertEquals(recupRound.getTeams().size(), round.getTeams().size(), "Team size is not correct");
    }
}
