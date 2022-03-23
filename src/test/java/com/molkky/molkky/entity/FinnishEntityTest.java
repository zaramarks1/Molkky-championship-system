package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Finnish;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.repository.FinnishRepository;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.RoundRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;

import javax.transaction.Transactional;
import java.util.Arrays;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class)
class FinnishEntityTest {
    @Autowired
    private FinnishRepository finnishRepository;
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private RoundRepository roundRepository;

    @Test
    @Transactional
    @Rollback(false)
    void testInsertSimpleGame() {
        Match match = new Match();
        Match match2 = new Match();
        List<Match> matches = Arrays.asList(match, match2);

        Finnish finnish = new Finnish(2, 2);
        match.setFinnish(finnish);
        match2.setFinnish(finnish);
        finnish.setMatches(matches);

        Round round = roundRepository.save(new Round("finnish", 2));
        finnish.setRound(round);

        finnish = finnishRepository.save(finnish);
        System.out.println(finnish.getMatches());
        Assertions.assertNotNull(finnish.getId());
        Match recupMatch = matchRepository.findById(match.getId());
        Assertions.assertEquals(recupMatch.getFinnish().getId(), finnish.getId());

        finnish = finnishRepository.findById(finnish.getId());
        Assertions.assertEquals(2, finnish.getMatches().size());
    }
}
