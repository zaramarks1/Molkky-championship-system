package com.molkky.molkky;

import com.molkky.molkky.domain.Court;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.repository.CourtRepository;
import com.molkky.molkky.repository.TeamRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = MolkkyApplication.class)
class CourtEntityTest {
    @Autowired
    private CourtRepository courtRepository;

    @Test
    void testInsertCourt() {
        Court court = courtRepository.save(new Court(true, "Court 1"));
        Assertions.assertEquals("Court 1", court.getName(), "Court name is not correct");
        Court recupTeam = courtRepository.findById(court.getId());
        Assertions.assertEquals("Court 1", recupTeam.getName(), "Court name is not correct");
    }
}
