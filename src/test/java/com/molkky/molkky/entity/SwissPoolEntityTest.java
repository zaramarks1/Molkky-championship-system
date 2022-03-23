package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.SwissPool;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.SwissPoolRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;

import javax.transaction.Transactional;
import java.util.Arrays;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class)
class SwissPoolEntityTest {
    @Autowired
    private SwissPoolRepository swissPoolRepository;
    @Autowired
    private MatchRepository matchRepository;

    @Test
    @Transactional
    @Rollback(false)
    void testInsertSimpleGame() {
        Match match = new Match();
        Match match2 = new Match();
        List<Match> matches = Arrays.asList(match, match2);

        SwissPool swissPool = new SwissPool(2, 2);
        match.setSwissPool(swissPool);
        match2.setSwissPool(swissPool);
        swissPool.setMatches(matches);

        swissPool = swissPoolRepository.save(swissPool);
        System.out.println(swissPool.getMatches());
        Assertions.assertNotNull(swissPool.getId());
        Match recupMatch = matchRepository.findById(match.getId());
        Assertions.assertEquals(recupMatch.getSwissPool().getId(), swissPool.getId());

        swissPool = swissPoolRepository.findById(swissPool.getId());
        Assertions.assertEquals(2, swissPool.getMatches().size());
    }
}
