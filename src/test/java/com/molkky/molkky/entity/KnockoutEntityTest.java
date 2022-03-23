package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Knockout;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.repository.KnockoutRepository;
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
class KnockoutEntityTest {
    @Autowired
    private KnockoutRepository knockoutRepository;
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

        Knockout knockout = new Knockout(2);
        match.setKnockout(knockout);
        match2.setKnockout(knockout);
        knockout.setMatches(matches);

        Round round = roundRepository.save(new Round("knockout", 2));
        knockout.setRound(round);

        knockout = knockoutRepository.save(knockout);
        System.out.println(knockout.getMatches());
        Assertions.assertNotNull(knockout.getId());
        Match recupMatch = matchRepository.findById(match.getId());
        Assertions.assertEquals(recupMatch.getKnockout().getId(), knockout.getId());

        knockout = knockoutRepository.findById(knockout.getId());
        Assertions.assertEquals(2, knockout.getMatches().size());
    }
}
