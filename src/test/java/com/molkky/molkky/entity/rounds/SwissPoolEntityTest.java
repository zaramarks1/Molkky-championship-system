package com.molkky.molkky.entity.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.SwissPool;
import com.molkky.molkky.repository.SetRepository;
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
    private SetRepository setRepository;
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private TournamentRepository tournamentRepository;

    @Test
    @Transactional
    @Rollback(false)
    void testInsertSimpleGame() {
        Set set = new Set();
        Set set2 = new Set();
        List<Set> sets = Arrays.asList(set, set2);

        SwissPool swissPool = new SwissPool(2, 2);
        set.setRound(swissPool);
        set2.setRound(swissPool);
        swissPool.setSets(sets);

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
        System.out.println(swissPool.getSets());
        Assertions.assertNotNull(swissPool.getId());
        Set recupSet = setRepository.findById(set.getId());
        Assertions.assertEquals(recupSet.getRound().getId(), swissPool.getId());

        swissPool = swissPoolRepository.findById(swissPool.getId());
        Assertions.assertEquals(2, swissPool.getSets().size());
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
