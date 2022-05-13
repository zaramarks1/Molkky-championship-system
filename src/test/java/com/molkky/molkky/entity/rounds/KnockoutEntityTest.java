package com.molkky.molkky.entity.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.Knockout;
import com.molkky.molkky.repository.KnockoutRepository;
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
class KnockoutEntityTest {
    @Autowired
    private KnockoutRepository knockoutRepository;
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

        Knockout knockout = new Knockout(2);
        match.setRound(knockout);
        match2.setRound(knockout);
        knockout.setMatches(matchs);

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

        knockout.setTournament(tournament);
        knockout = knockoutRepository.save(knockout);
        System.out.println(knockout.getMatches());
        Assertions.assertNotNull(knockout.getId());
        Match recupMatch = matchRepository.findById(match.getId());
        Assertions.assertEquals(recupMatch.getRound().getId(), knockout.getId());

        knockout = knockoutRepository.findById(knockout.getId());
        Assertions.assertEquals(2, knockout.getMatches().size());
    }
}
