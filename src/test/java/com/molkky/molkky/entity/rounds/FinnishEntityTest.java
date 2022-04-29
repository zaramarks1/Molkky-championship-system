package com.molkky.molkky.entity.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.Finnish;
import com.molkky.molkky.repository.FinnishRepository;
import com.molkky.molkky.repository.MatchRepository;
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
class FinnishEntityTest {
    @Autowired
    private FinnishRepository finnishRepository;
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private TournamentRepository tournamentRepository;

    @Test
    @Transactional
    @Rollback(false)
    void testInsertFinnishGame() {
        Match match = new Match();
        Match match2 = new Match();
        List<Match> matchs = Arrays.asList(match, match2);

        Finnish finnish = new Finnish(2, 2);
        match.setRound(finnish);
        match2.setRound(finnish);
        finnish.setMatches(matchs);

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

        finnish.setTournament(tournament);
        finnish = finnishRepository.save(finnish);
        System.out.println(finnish.getMatches());
        Assertions.assertNotNull(finnish.getId());
        Match recupMatch = matchRepository.findById(match.getId());
        Assertions.assertEquals(recupMatch.getRound().getId(), finnish.getId());

        finnish = finnishRepository.findById(finnish.getId());
        Assertions.assertEquals(2, finnish.getMatches().size());
    }
}
