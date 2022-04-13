package com.molkky.molkky.entity;

import Type.UserRole;
import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.RoundRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;

import javax.transaction.Transactional;
import java.text.ParseException;
import java.util.*;

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
        User user = userRepository.save(new User("pseudoUser1", "surname1", "forename1", "club1", "email1", false, UserRole.ADM));
        Set<User> admins = new HashSet<>();
        admins.add(user);
        tournament.setUsers(admins);
        tournamentRepository.save(tournament);

        Assertions.assertEquals(1, tournament.getUsers().size(), "Tournament should have 1 admin");
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
        List<Round> rounds = new ArrayList<>();
        rounds.add(round);
        tournament.setRounds(rounds);
        tournamentRepository.save(tournament);

        Assertions.assertEquals(1, tournament.getRounds().size(), "Tournament should have 1 round");
    }

    @Test
    void testCreateTournamentWithModel() throws ParseException {
        TournamentModel tournamentModel = new TournamentModel();
        tournamentModel.setName("tournament_name");
        tournamentModel.setLocation("location");
        tournamentModel.setDate("01-01-2020");
        tournamentModel.setCutOffDate("01-01-2020");
        tournamentModel.setMaxTeam(1);
        tournamentModel.setMinTeam(2);
        tournamentModel.setNbRounds(2);
        tournamentModel.setNbCourts(2);
        tournamentModel.setVisible(true);
        Tournament tournament = new Tournament(tournamentModel);
        Assertions.assertEquals("tournament_name", tournament.getName(), "Tournament name should be tournament_name");
        Assertions.assertEquals("location", tournament.getLocation(), "Tournament location should be location");
        Assertions.assertEquals(1, tournament.getMaxTeam(), "Tournament maxTeam should be 1");
        Assertions.assertEquals(2, tournament.getMinTeam(), "Tournament minTeam should be 2");
        Assertions.assertEquals(2, tournament.getNbRounds(), "Tournament nbRounds should be 2");
        Assertions.assertEquals(2, tournament.getNbCourts(), "Tournament nbCounts should be 2");
        Assertions.assertTrue(tournament.isVisible(), "Tournament visible should be true");
    }
}
