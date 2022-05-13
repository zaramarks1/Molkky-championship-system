package com.molkky.molkky.entity.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.PoolRepository;
import com.molkky.molkky.repository.RoundRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;

import javax.transaction.Transactional;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class)
class PoolEntityTest {
    @Autowired
    private PoolRepository poolRepository;
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private TournamentRepository tournamentRepository;

    @Test
    @Transactional
    @Rollback(false)
    void testInsertPool() {
        Match match = new Match();
        Match match2 = new Match();
        List<Match> matchs = Arrays.asList(match, match2);


        Pool pool = new Pool();
        match.setRound(pool);
        match2.setRound(pool);
        pool.setMatches(matchs);

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

        pool.setTournament(tournament);
        pool = roundRepository.save(pool);
        System.out.println(pool.getMatches());
        Assertions.assertNotNull(pool.getId());
        Match recupMatch = matchRepository.findById(match.getId());
        Assertions.assertEquals(recupMatch.getRound().getId(), pool.getId());

        pool = (Pool) roundRepository.findById(pool.getId());
        Assertions.assertEquals(2, pool.getMatches().size());
    }
}
