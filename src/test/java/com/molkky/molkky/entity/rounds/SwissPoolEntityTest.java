package com.molkky.molkky.entity.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.SwissPool;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.RoundRepository;
import com.molkky.molkky.repository.SwissPoolRepository;
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
class SwissPoolEntityTest {
    @Autowired
    private SwissPoolRepository swissPoolRepository;
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private TournamentRepository tournamentRepository;

    @Test
    @Transactional
    @Rollback(false)
    void testInsertSimpleGame() {
        Match match = new Match();
        Match match2 = new Match();
        List<Match> matchs = Arrays.asList(match, match2);

        SwissPool swissPool = new SwissPool(2, 2);
        match.setRound(swissPool);
        match2.setRound(swissPool);
        swissPool.setMatches(matchs);

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

        swissPool.setTournament(tournament);
        swissPool = swissPoolRepository.save(swissPool);
        System.out.println(swissPool.getMatches());
        Assertions.assertNotNull(swissPool.getId());
        Match recupMatch = matchRepository.findById(match.getId());
        Assertions.assertEquals(recupMatch.getRound().getId(), swissPool.getId());

        swissPool = swissPoolRepository.findById(swissPool.getId());
        Assertions.assertEquals(2, swissPool.getMatches().size());
    }

    @Test
    void testCast() {
        SwissPool swissPool = new SwissPool(2, 2);
        swissPool.setNbTeamsQualified(4);
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
        swissPool.setTournament(tournament);
        swissPoolRepository.save(swissPool);
        SwissPool nvPool = (SwissPool) roundRepository.findById(swissPool.getId());
        Assertions.assertEquals(4, nvPool.getNbTeamsQualified());
    }
}
