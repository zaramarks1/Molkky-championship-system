package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.RoundRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;

import javax.transaction.Transactional;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

@SpringBootTest(classes = MolkkyApplication.class)
class TournamentEntityTest {
    @Autowired
    private TournamentRepository tournamentRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private RoundRepository roundRepository;

    @Test
    void testInsertTournament() {
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

        Assertions.assertEquals("tournament_name", tournament.getName(), "Tournament name should be tournament_name");

        Tournament tournament2 = tournamentRepository.findById(tournament.getId());
        Assertions.assertEquals("tournament_name", tournament2.getName(), "Tournament name should be tournament_name");
    }

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithAdmins() {
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
        User user = userRepository.save(new User("pseudoUser1", "surname1", "forename1", "club1", "email1", false));
        Set<User> admins = new HashSet<>();
        admins.add(user);
        tournament.setAdmins(admins);
        tournamentRepository.save(tournament);

        Assertions.assertEquals(1, tournament.getAdmins().size(), "Tournament should have 1 admin");
    }

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithRounds() {
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

        Round round = new Round("finnish", 2);
        round.setTournament(tournament);
        roundRepository.save(round);
        Set<Round> rounds = new HashSet<>();
        rounds.add(round);
        tournament.setRounds(rounds);
        tournamentRepository.save(tournament);

        Assertions.assertEquals(1, tournament.getRounds().size(), "Tournament should have 1 round");
    }
}
