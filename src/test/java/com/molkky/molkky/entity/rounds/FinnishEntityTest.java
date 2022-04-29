package com.molkky.molkky.entity.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.Finnish;
import com.molkky.molkky.repository.FinnishRepository;
import com.molkky.molkky.repository.SetRepository;
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
    private SetRepository setRepository;
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private TournamentRepository tournamentRepository;

    @Test
    @Transactional
    @Rollback(false)
    void testInsertFinnishGame() {
        Set set = new Set();
        Set set2 = new Set();
        List<Set> sets = Arrays.asList(set, set2);

        Finnish finnish = new Finnish(2, 2);
        set.setRound(finnish);
        set2.setRound(finnish);
        finnish.setSets(sets);

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
        System.out.println(finnish.getSets());
        Assertions.assertNotNull(finnish.getId());
        Set recupSet = setRepository.findById(set.getId());
        Assertions.assertEquals(recupSet.getRound().getId(), finnish.getId());

        finnish = finnishRepository.findById(finnish.getId());
        Assertions.assertEquals(2, finnish.getSets().size());
    }
}
