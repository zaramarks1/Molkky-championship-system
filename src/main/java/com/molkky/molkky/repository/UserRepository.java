package com.molkky.molkky.repository;

import com.molkky.molkky.domain.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.List;


@Repository
public interface UserRepository extends JpaRepository<User, String>, JpaSpecificationExecutor<User> {
    User findById(Integer id);
    List<User> findAll();
    User findUserByEmail(String email);
    User findUserByEmailAndPassword(String email, String password);


    boolean existsUserByEmailAndPassword(String email, String password);
    boolean existsUserByEmail(String email);
}
